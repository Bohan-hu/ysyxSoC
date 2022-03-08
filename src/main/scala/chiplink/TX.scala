// See LICENSE for license details.
package sifive.blocks.devices.chiplink

import Chisel.{defaultCompileOptions => _, _}
import freechips.rocketchip.util.CompileOptions.NotStrictInferReset
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util._

class TX(info: ChipLinkInfo) extends Module
{
  val io = new Bundle {
    val c2b_clk  = Clock(OUTPUT)
    val c2b_rst  = Bool(OUTPUT)
    val c2b_send = Bool(OUTPUT)
    val c2b_data = UInt(OUTPUT, info.params.dataBits)
    // 非同步的信号
    val a = new AsyncBundle(new DataLayer(info.params), info.params.crossing).flip
    val b = new AsyncBundle(new DataLayer(info.params), info.params.crossing).flip
    val c = new AsyncBundle(new DataLayer(info.params), info.params.crossing).flip
    val d = new AsyncBundle(new DataLayer(info.params), info.params.crossing).flip
    val e = new AsyncBundle(new DataLayer(info.params), info.params.crossing).flip
    // 同步的信号
    val sa = DecoupledIO(new DataLayer(info.params)).flip
    val sb = DecoupledIO(new DataLayer(info.params)).flip
    val sc = DecoupledIO(new DataLayer(info.params)).flip
    val sd = DecoupledIO(new DataLayer(info.params)).flip
    val se = DecoupledIO(new DataLayer(info.params)).flip
    // TODO: 什么是Credit (Done)
    val rxc = new AsyncBundle(new CreditBump(info.params), AsyncQueueParams.singleton()).flip
    val txc = new AsyncBundle(new CreditBump(info.params), AsyncQueueParams.singleton()).flip
  }

  // Currently available credits
  // 目前可用的Credit
  val rx = RegInit(CreditBump(info.params, 0))
  val tx = RegInit(CreditBump(info.params, 0))

  // Constantly pull credits from RX
  // 从Rx轮训Credits
  // rxInCredits
  val rxInc = FromAsyncBundle(io.rxc)
  val txInc = FromAsyncBundle(io.txc)
  rxInc.ready := Bool(true)
  txInc.ready := Bool(true)

  // Cross the requests (if necessary)
  val sync = info.params.syncTX
  // Use different Queue according to whether it is sync
  // 输入的通道a,b,c,d,e
  val qa = if (sync) ShiftQueue(io.sa, 2) else FromAsyncBundle(io.a)
  val qb = if (sync) ShiftQueue(io.sb, 2) else FromAsyncBundle(io.b)
  val qc = if (sync) ShiftQueue(io.sc, 2) else FromAsyncBundle(io.c)
  val qd = if (sync) ShiftQueue(io.sd, 2) else FromAsyncBundle(io.d)
  val qe = if (sync) ShiftQueue(io.se, 2) else FromAsyncBundle(io.e)
  private def qX = Seq(qa, qb, qc, qd, qe)

  // Consume TX credits and propagate pre-paid requests
  // 先把TX这边的Credit给使用了，然后把request传下去
  // qX: 所有队列 
  // 每一个通道都有20个Credits bits
  val ioX = (qX zip (tx.X zip txInc.bits.X)) map { case (q, (credit, gain)) =>
    // Example: (tilelink bundle, (credits bits for all channel, txInCredits for all channel))
    // 也就是tilelink通道，模块维护的每个通道的credits和得到的credits
    // 全部传输完成之后拉高first
    val first = RegEnable(q.bits.last, Bool(true), q.fire())
    // 剩下的credit数量
    val delta = credit -& q.bits.beats
    // 是否允许进入队列
    val allow = !first || (delta.asSInt >= SInt(0))
    // 更新该通道剩余的Credit（饱和计数）
    credit := Mux(q.fire() && first, delta, credit) + Mux(txInc.fire(), gain, UInt(0))

    val cq = Module(new ShiftQueue(q.bits.cloneType, 2)) // maybe flow?
    cq.io.enq.bits := q.bits
    cq.io.enq.valid := q.valid && allow
    q.ready := cq.io.enq.ready && allow
    cq.io.deq
  }

  // Prepare RX credit update headers
  // 更新对面端的RX Credit的消息
  // 如何去封装header呢?
  // rxQ是最终发送出去的东西
  // “贪心”地每一次尽可能地归还最大的credit（2的n次方）
  val rxQ = Module(new ShiftQueue(new DataLayer(info.params), 2)) // maybe flow?
  val (rxHeader, rxLeft) = rx.toHeader
  // 不停地往rxq里塞东西，塞更新后的header
  rxQ.io.enq.valid := Bool(true)
  rxQ.io.enq.bits.data  := rxHeader
  rxQ.io.enq.bits.last  := Bool(true)
  rxQ.io.enq.bits.beats := UInt(1)
  // 更新Credits
  rx := Mux(rxQ.io.enq.fire(), rxLeft, rx) + Mux(rxInc.fire(), rxInc.bits, CreditBump(info.params, 0))

  // Include the F credit channel in arbitration
  val f = Wire(rxQ.io.deq)
  val ioF = ioX :+ f
  val requests = Cat(ioF.map(_.valid).reverse)
  val lasts = Cat(ioF.map(_.bits.last).reverse)

  // How often should we force transmission of a credit update? sqrt
  val xmitBits = log2Ceil(info.params.Qdepth) / 2
  val xmit = RegInit(UInt(0, width = xmitBits))
  val forceXmit = xmit === UInt(0)
  when (!forceXmit) { xmit := xmit - UInt(1) }
  when (f.fire()) { xmit := ~UInt(0, width = xmitBits) }

  // Flow control for returned credits
  val allowReturn = !ioX.map(_.valid).reduce(_ || _) || forceXmit
  f.bits  := rxQ.io.deq.bits
  f.valid := rxQ.io.deq.valid && allowReturn
  rxQ.io.deq.ready := f.ready && allowReturn

  // Select a channel to transmit from those with data and space
  // 仲裁：从6个通道里选一个发送出去
  val first = RegInit(Bool(true))
  // state: 目前正在发送的那个通道(ABCDEF之一，所以是6位)
  val state = Reg(UInt(0, width=6))
  // 从6个request里选一个，first是上次被选中的那个会参与？
  val readys = TLArbiter.roundRobin(6, requests, first)
  // winnder：被选中的通道，用one hot编码
  val winner = readys & requests
  // grant: 被选中的，因为winner是组合逻辑，state是时序逻辑，这个只是起到了一个hold的作用而已
  // grant = hold(winner)
  val grant = Mux(first, winner, state)
  // allowed = hold(state)
  val allowed = Mux(first, readys, state)
  // 被仲裁选中允许传输的队列
  (ioF zip allowed.asBools) foreach { case (beat, sel) => beat.ready := sel }

  val send = Mux(first, rxQ.io.deq.valid, (state & requests) =/= UInt(0))
  assert (send === ((grant & requests) =/= UInt(0)))

  when (send) { first := (grant & lasts).orR }
  when (first) { state := winner }

  // Form the output beat
  io.c2b_clk  := clock
  io.c2b_rst  := AsyncResetReg(Bool(false), clock, reset, true, None)
  io.c2b_send := RegNext(RegNext(send, Bool(false)), Bool(false))
  io.c2b_data := RegNext(Mux1H(RegNext(grant), RegNext(Vec(ioF.map(_.bits.data)))))
}
