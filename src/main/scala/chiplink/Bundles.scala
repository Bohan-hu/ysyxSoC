// See LICENSE for license details.
package sifive.blocks.devices.chiplink

import Chisel.{defaultCompileOptions => _, _}
import freechips.rocketchip.util.CompileOptions.NotStrictInferReset
import freechips.rocketchip.util.{rightOR,GenericParameterizedBundle}

class WideDataLayerPortLane(params: ChipLinkParams) extends GenericParameterizedBundle(params) {
  // 单侧的通道
  val clk  = Clock(OUTPUT)
  val rst  = Bool(OUTPUT)
  val send = Bool(OUTPUT)
  val data = UInt(OUTPUT, width=params.dataBits)
}

class WideDataLayerPort(params: ChipLinkParams) extends GenericParameterizedBundle(params) {
  // 这个是定义在顶层的数据接口
  // FPGA -> CPU和CPU -> FPGA
  // 每一个接口都是上面那个WideDataLayerPortLane的形式
  val c2b = new WideDataLayerPortLane(params)
  val b2c = new WideDataLayerPortLane(params).flip
}

class DataLayer(params: ChipLinkParams) extends GenericParameterizedBundle(params) {
  // 数据层，32位的数据
  val data = UInt(OUTPUT, width=params.dataBits)
  // 1位的last信号
  val last = Bool(OUTPUT)
  // x = trans
  // 已经传输的节拍数量
  val beats = UInt(OUTPUT, width=params.xferBits + 1)
}

class CreditBump(params: ChipLinkParams) extends GenericParameterizedBundle(params) {
  // 加上Credit后封装？
  // CreditBits目前是20，意思说有20M的Buffer项可以用
  val a = UInt(OUTPUT, width = params.creditBits)
  val b = UInt(OUTPUT, width = params.creditBits)
  val c = UInt(OUTPUT, width = params.creditBits)
  val d = UInt(OUTPUT, width = params.creditBits)
  val e = UInt(OUTPUT, width = params.creditBits)
  def X: Seq[UInt] = Seq(a, b, c, d, e)

  // saturating addition
  // 饱和计数器
  def +(that: CreditBump): CreditBump = {
    val out = Wire(new CreditBump(params))
    (out.X zip (X zip that.X)) foreach { case (o, (x, y)) =>
      val z = x +& y
      o := Mux((z >> params.creditBits).orR, ~UInt(0, width=params.creditBits), z)
    }
    out
  }

  def msb(x: UInt) = {
    // rightOR: 找到最左边的1，然后把右边全部填为1
    val mask = rightOR(x) >> 1
    // maskOH: 把最高位的1找出来，其他的清除，是一个one-hot编码
    val msbOH = ~(~x | mask)
    val msb = OHToUInt(msbOH << 1, params.creditBits + 1) // 0 = 0, 1 = 1, 2 = 4, 3 = 8, ...
    // 把位宽拓宽到5位
    val pad = (msb | UInt(0, width=5))(4,0)
    // 返回pad后的5位msb标志，和清除了msb的x
    (pad, x & mask)
  }
  // Send the MSB of the credits
  def toHeader: (UInt, CreditBump) = {
    // 猜测一次清除掉x的一个msb，然后返回剩下的部分(x_rest)
    // 例如，01011,toHeader之后，就会返回3和00011
    val (a_msb, a_rest) = msb(a)
    val (b_msb, b_rest) = msb(b)
    val (c_msb, c_rest) = msb(c)
    val (d_msb, d_rest) = msb(d)
    val (e_msb, e_rest) = msb(e)
    val header = Cat(
      e_msb, d_msb, c_msb, b_msb, a_msb,
      UInt(0, width = 4), // padding
      UInt(5, width = 3))

    val out = Wire(new CreditBump(params))
    out.a := a_rest
    out.b := b_rest
    out.c := c_rest
    out.d := d_rest
    out.e := e_rest
    (header, out)
  }
  def toSimpleHeader: (UInt, CreditBump) = {
    def msbOH(x: UInt) = {
      val mask = rightOR(x) >> 1
      ~(~x | mask)
    }
    // Find all msb and OR them into a single vector
    val msbOR = X.map(msbOH(_)).reduce(_|_)
    // Find the biggest msb
    val msb2 = msbOH(msbOR)
    // Find the owner of the biggst msb
    val candidate = Wire(UInt(3.W))
    candidate := 7.U
    for ( i <- 0 until 5 ) {
      val mask = X(i) & msb2
      when(mask.orR()) {
        candidate := i.U
      }
    }
    // assert(candidate =/= 7.U)
    val (a_msb, a_rest) = msb(a)
    val (b_msb, b_rest) = msb(b)
    val (c_msb, c_rest) = msb(c)
    val (d_msb, d_rest) = msb(d)
    val (e_msb, e_rest) = msb(e)

    val header = Cat(
      Mux(candidate === 4.U, e_msb, 0.U),
      Mux(candidate === 3.U, d_msb, 0.U),
      Mux(candidate === 2.U, c_msb, 0.U),
      Mux(candidate === 1.U, b_msb, 0.U),
      Mux(candidate === 0.U, a_msb, 0.U),
      UInt(0, width = 4), // padding
      UInt(5, width = 3))

    val out = Wire(new CreditBump(params))
    out.a := Mux(candidate === 0.U, a_rest, a)
    out.b := Mux(candidate === 1.U, b_rest, b)
    out.c := Mux(candidate === 2.U, c_rest, c)
    out.d := Mux(candidate === 3.U, d_rest, d)
    out.e := Mux(candidate === 4.U, e_rest, e)
    (header, out)
  }
}

object CreditBump {
  def apply(params: ChipLinkParams, x: Int): CreditBump = {
    // 指定Credit的初始值，初始化每个通道对应的Credit，然后返回一个Credit Bump
    val v = UInt(x, width = params.creditBits)
    val out = Wire(new CreditBump(params))
    out.X.foreach { _ := v }
    out
  }

  def apply(params: ChipLinkParams, header: UInt): CreditBump = {
    // 把header的credit转换过来
    // 和toHeader互为反函数
    def convert(x: UInt) =
      Mux(x > UInt(params.creditBits),
          ~UInt(0, width = params.creditBits),
          UIntToOH(x, params.creditBits + 1) >> 1)
    val out = Wire(new CreditBump(params))
    out.a := convert(header(11,  7))
    out.b := convert(header(16, 12))
    out.c := convert(header(21, 17))
    out.d := convert(header(26, 22))
    out.e := convert(header(31, 27))
    out
  }
}
