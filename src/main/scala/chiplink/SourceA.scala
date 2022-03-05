// See LICENSE for license details.
package sifive.blocks.devices.chiplink

import Chisel.{defaultCompileOptions => _, _}
import freechips.rocketchip.util.CompileOptions.NotStrictInferReset
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

// 这个SourceA指的是主动发请求出去的，是从Chiplink那边收到数据包之后用来解包的
// TX -> SourceA -> A通道输出
class SourceA(info: ChipLinkInfo) extends Module
{
  val io = new Bundle {
    // A 通道，是一个输出通道，输出解包后的结果
    val a = Decoupled(new TLBundleA(info.edgeOut.bundle))
    // 从外部队列的输入
    val q = Decoupled(UInt(width = info.params.dataBits)).flip
    // Used by D to find the txn
    // 当D通道回应了A通道的请求时，可以将CAM里缓存的A项给清除了
    // TL指的是TileLink
    // TODO: 为什么需要两个d通道输入？
    val d_tlSource = Valid(UInt(width = info.params.sourceBits)).flip
    // CL指的是Chiplink
    val d_clSource = UInt(OUTPUT, width = info.params.clSourceBits)
  }

  // CAM of sources used for each domain
  // A请求的每一个Domain都具有CAM，用于暂存
  val cams = Seq.fill(info.params.domains) {
    Module(new CAM(info.params.sourcesPerDomain, info.params.clSourceBits))
  }

  // A simple FSM to generate the packet components
  // 将A通道的请求拆包发送
  // TODO 如果要改成8位的，这里的状态就需要改变
  val state = RegInit(UInt(0, width = 2))
  val s_header   = UInt(0, width = 2)
  val s_address0 = UInt(1, width = 2)
  val s_address1 = UInt(2, width = 2)
  val s_data     = UInt(3, width = 2)

  // 当状态转到Key的时候，捕捉并输出这个值，直到下一次达到这个状态（Hold）
  private def hold(key: UInt)(data: UInt) = {
    val enable = state === key
    Mux(enable, data, RegEnable(data, enable))
  }

  // Extract header fields
  // 将TileLink头部部分提取出来
  // 当状态是s_header的时候，hold住这些请求信息
  // 这里的q是TileLink请求的Queue
  val Seq(_, q_opcode, q_param, q_size, q_domain, q_source) =
    info.decode(io.q.bits).map(hold(s_header) _)

  // Latch address
  val q_address0 = hold(s_address0)(io.q.bits)
  val q_address1 = hold(s_address1)(io.q.bits)

  // 是否发送到最后一个数据（A通道突发写，需要hasData）
  val (_, q_last) = info.firstlast(io.q, Some(UInt(0)))
  // 是否是写操作
  val q_hasData = !q_opcode(2)
  // TODO
  val a_first = RegEnable(state =/= s_data, io.q.fire())

  when (io.q.fire()) {
    switch (state) {
      // 一拍发一个出去
      is (s_header)   { state := s_address0 }
      is (s_address0) { state := s_address1 }
      // 如果有数据，继续处理数据，如果没有数据，则去发请求
      is (s_address1) { state := Mux(q_hasData, s_data, s_header) }
      is (s_data)     { state := Mux(!q_last,   s_data, s_header) }
    }
  }

  // Determine if the request is legal. If not, route to error device.
  // 如果这个请求不合法，那么就把他扔到error device里
  val q_address = Cat(q_address1, q_address0)
  val q_acq = q_opcode === TLMessages.AcquireBlock || q_opcode === TLMessages.AcquirePerm
  val q_write = Mux(q_acq, q_param === TLPermissions.NtoT || q_param === TLPermissions.BtoT, q_hasData)
  val exists = info.edgeOut.manager.containsSafe(q_address)
  private def writeable(m: TLManagerParameters): Boolean = if (m.supportsAcquireB) m.supportsAcquireT else m.supportsPutFull
  private def acquireable(m: TLManagerParameters): Boolean = m.supportsAcquireB || m.supportsAcquireT
  private def toBool(x: Boolean) = Bool(x)
  val writeOk = info.edgeOut.manager.fastProperty(q_address, writeable, toBool)
  val acquireOk = info.edgeOut.manager.fastProperty(q_address, acquireable, toBool)
  val q_legal = exists && (!q_write || writeOk) && (!q_acq || acquireOk)

  // Look for an available source in the correct domain
  val source_ok = Vec(cams.map(_.io.alloc.ready))(q_domain)
  val source    = Vec(cams.map(_.io.key))(q_domain) holdUnless a_first
  val a_sel = UIntToOH(q_domain)

  // Feed our preliminary A channel via the Partial Extractor FSM
  val extract = Module(new ParitalExtractor(io.a.bits))
  io.a <> extract.io.o
  val a = extract.io.i
  extract.io.last := q_last

  // q_opcode是之前从队列中hold过来的信号
  a.bits.opcode  := q_opcode
  a.bits.param   := q_param
  a.bits.size    := q_size
  a.bits.source  := Cat(q_domain, source)
  a.bits.address := info.makeError(q_legal, q_address)
  // 从地址的地位算出来Mask
  a.bits.mask    := MaskGen(q_address0, q_size, info.params.dataBytes)
  a.bits.data    := io.q.bits
  a.bits.corrupt := Bool(false)

  // Stall: 
  //
  val stall = a_first && !source_ok
  val xmit = q_last || state === s_data
  a.valid := (io.q.valid && !stall) &&  xmit
  io.q.ready := (a.ready && !stall) || !xmit
  // 把对应的域写到CAM里面
  (cams zip a_sel.asBools) foreach { case (cam, sel) =>
    cam.io.alloc.valid := sel && a_first && xmit && io.q.valid && a.ready
    cam.io.alloc.bits  := q_source
  }

  // Free the CAM entries
  val d_clDomain = io.d_tlSource.bits >> log2Ceil(info.params.sourcesPerDomain)
  val d_sel = UIntToOH(d_clDomain)
  io.d_clSource := Vec(cams.map(_.io.data))(d_clDomain)
  (cams zip d_sel.asBools) foreach { case (cam, sel) =>
    cam.io.free.bits  := io.d_tlSource.bits
    cam.io.free.valid := io.d_tlSource.valid && sel
  }
}
