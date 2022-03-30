// See LICENSE.SiFive for license details.

package freechips.rocketchip.system

import chisel3._
import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.devices.debug.Debug
import freechips.rocketchip.util.AsyncResetReg
import freechips.rocketchip.amba.axi4._
import ysyx._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.subsystem._

// class TestHarness()(implicit p: Parameters) extends Module {
//   val io = IO(new Bundle { })
//   val ldut = LazyModule(new ysyxSoCFull)
//   val dut = Module(ldut.module)
//   dut.dontTouchPorts()
// }

class TestHarness()(implicit p: Parameters) extends LazyModule {
  val ldut = LazyModule(new FPGATop)
  val memNode = AXI4MasterNode(p(ExtIn).map(params =>
    AXI4MasterPortParameters(
      masters = Seq(AXI4MasterParameters(
        name = "mem",
        id   = IdRange(0, 1 << ChipLinkParam.idBits))))).toSeq)
  val mmioNode = AXI4MasterNode(p(ExtIn).map(params =>
    AXI4MasterPortParameters(
      masters = Seq(AXI4MasterParameters(
        name = "mmio",
        id   = IdRange(0, 1 << ChipLinkParam.idBits))))).toSeq)
  val xbar = AXI4Xbar()
  xbar := memNode
  xbar := mmioNode
  val dummySlv = AXI4SlaveNodeGenerator(p(ExtBus), ChipLinkParam.allSpace)
  dummySlv := xbar
  // SimMem := xbar
  // apbBridge := xbar
  lazy val module = new LazyModuleImp(this) {
    val dut = ldut.module
    dut.clockFPGA := clock
    dut.resetFPGA := reset
    dut.meip := 0.U
    dut.dontTouchPorts()
    val (mem, _) = memNode.out(0)
    val (mmio, _) = mmioNode.out(0)
    // Connect AXI signals
    mem <> dut.io_master_mem
    mmio <> dut.io_master_mmio
    // Don't connect Slaves
    dut.io_slave.tieoff()
  }
}

class TestHarness2()(implicit p: Parameters) extends Module {
  val io = IO(new Bundle {
    val success = Output(Bool())
  })

  val ldut = LazyModule(new ExampleRocketSystem)
  val dut = Module(ldut.module)

  // Allow the debug ndreset to reset the dut, but not until the initial reset has completed
  dut.reset := (reset.asBool | dut.debug.map { debug => AsyncResetReg(debug.ndreset) }.getOrElse(false.B)).asBool

  dut.dontTouchPorts()
  dut.tieOffInterrupts()
  SimAXIMem.connectMem(ldut)
  SimAXIMem.connectMMIO(ldut)
  ldut.l2_frontend_bus_axi4.foreach(_.tieoff)
  Debug.connectDebug(dut.debug, dut.resetctrl, dut.psd, clock, reset.asBool, io.success)
}
