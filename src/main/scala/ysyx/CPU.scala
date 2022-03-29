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
    val io_meip = Input(UInt(3.W))
    val io_mem = AXI4Bundle(CPUAXI4BundleParameters())
    val io_mmio = AXI4Bundle(CPUAXI4BundleParameters())
    val io_frontend = Flipped(AXI4Bundle(CPUAXI4BundleParameters()))
  })
}

class CPU(idBits: Int)(implicit p: Parameters) extends LazyModule {
  val memNode = AXI4MasterNode(p(ExtIn).map(params =>
    AXI4MasterPortParameters(
      masters = Seq(AXI4MasterParameters(
        name = "cpu",
        id   = IdRange(0, 1 << idBits))))).toSeq)
  val mmioNode = AXI4MasterNode(p(ExtIn).map(params =>
    AXI4MasterPortParameters(
      masters = Seq(AXI4MasterParameters(
        name = "cpu",
        id   = IdRange(0, 1 << idBits))))).toSeq)
  lazy val module = new LazyModuleImp(this) {
    val (mem, _) = memNode.out(0)
    val (mmio, _) = mmioNode.out(0)
    val meip = IO(Input(UInt(3.W)))
    val slave = IO(Flipped(AXI4Bundle(CPUAXI4BundleParameters())))

    val cpu = Module(new NutShell)
    cpu.io.clock := clock
    cpu.io.reset := reset
    cpu.io.io_meip := meip
    cpu.io.io_frontend <> slave
    mem <> cpu.io.io_mem
    mmio <> cpu.io.io_mmio
  }
}
