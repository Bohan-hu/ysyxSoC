package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.{Parameters}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.util._

object CPUAXI4BundleParameters {
  def apply() = AXI4BundleParameters(addrBits = 32, dataBits = 64, idBits = ChipLinkParam.idBits)
}

class NutShell extends BlackBox {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val reset = Input(Reset())
    val io_interrupt = Input(UInt(3.W))
    val io_master = AXI4Bundle(CPUAXI4BundleParameters())
    // If has MMIO
    val io_mmio = AXI4Bundle(CPUAXI4BundleParameters())
    // Endif
    val io_slave = Flipped(AXI4Bundle(CPUAXI4BundleParameters()))
  })
}

class CPU(idBits: Int)(implicit p: Parameters) extends LazyModule {
  val masterNode = AXI4MasterNode(p(ExtIn).map(params =>
    AXI4MasterPortParameters(
      masters = Seq(AXI4MasterParameters(
        name = "cpu",
        id   = IdRange(0, 1))))).toSeq)
  // If has MMIO
  val mmioNode = AXI4MasterNode(p(ExtIn).map(params =>
    AXI4MasterPortParameters(
      masters = Seq(AXI4MasterParameters(
        name = "mmio",
        id   = IdRange(1, 2))))).toSeq)
  // Endif
  lazy val module = new LazyModuleImp(this) {
    val (master, _) = masterNode.out(0)
    // If has MMIO
    val (mmio, _) = mmioNode.out(0)
    // Endif
    val interrupt = IO(Input(UInt(3.W)))
    val slave = IO(Flipped(AXI4Bundle(CPUAXI4BundleParameters())))

    val cpu = Module(new NutShell)
    cpu.io.clock := clock
    cpu.io.reset := reset
    cpu.io.io_interrupt := interrupt
    cpu.io.io_slave <> slave
    master <> cpu.io.io_master
    assert(!cpu.io.io_master.aw.fire())
    // If has MMIO
    mmio <> cpu.io.io_mmio
    // Endif
  }
}
