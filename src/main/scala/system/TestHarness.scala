// See LICENSE.SiFive for license details.

package freechips.rocketchip.system

import chisel3._
import freechips.rocketchip.config.Parameters
import freechips.rocketchip.devices.debug.Debug
import freechips.rocketchip.diplomacy.LazyModule
import freechips.rocketchip.util.AsyncResetReg
import freechips.rocketchip.amba.axi4._
import ysyx._

// class TestHarness()(implicit p: Parameters) extends Module {
//   val io = IO(new Bundle { })
//   val ldut = LazyModule(new ysyxSoCFull)
//   val dut = Module(ldut.module)
//   dut.dontTouchPorts()
// }

class TestHarness()(implicit p: Parameters) extends Module {
  val io = IO(new Bundle { 
    val clockFPGA = Input(Clock())
    val resetFPGA = Input(Bool())
    val master_mem = AXI4Bundle(CPUAXI4BundleParameters())
    val master_mmio = AXI4Bundle(CPUAXI4BundleParameters())
    val slave = Flipped(AXI4Bundle(CPUAXI4BundleParameters()))
    val meip = Input(UInt(3.W))
  })
  val ldut = LazyModule(new FPGATop)
  val dut = Module(ldut.module)
  dut.clockFPGA := io.clockFPGA
  dut.resetFPGA := io.resetFPGA
  dut.meip := io.meip
  dut.dontTouchPorts()
  // Connect AXI signals
  io.master_mem <> dut.io_master_mem
  io.master_mmio <> dut.io_master_mmio
  dut.io_slave <> io.slave
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
