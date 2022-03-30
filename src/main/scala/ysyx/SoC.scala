package ysyx

import chisel3._
import chisel3.util._

import freechips.rocketchip.diplomacy._
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.subsystem._
import freechips.rocketchip.util._
import freechips.rocketchip.amba.axi4._
import freechips.rocketchip.amba.apb._
import freechips.rocketchip.system.SimAXIMem

object AXI4SlaveNodeGenerator {
  def apply(params: Option[MasterPortParams], address: Seq[AddressSet])(implicit valName: ValName) =
    AXI4SlaveNode(params.map(p => AXI4SlavePortParameters(
        slaves = Seq(AXI4SlaveParameters(
          address       = address,
          executable    = p.executable,
          supportsWrite = TransferSizes(1, p.maxXferBytes),
          supportsRead  = TransferSizes(1, p.maxXferBytes))),
        beatBytes = p.beatBytes
      )).toSeq)
}

class ysyxSoCASIC(implicit p: Parameters) extends LazyModule {
  val chipMaster = LazyModule(new ChipLinkMaster)
  val xbar = AXI4Xbar()
  val cpu = LazyModule(new CPU(idBits = ChipLinkParam.idBits))
  val chiplinkNode = AXI4SlaveNodeGenerator(p(ExtBus), ChipLinkParam.allSpace)

  chiplinkNode := xbar
  xbar := cpu.memNode
  xbar := cpu.mmioNode

  override lazy val module = new LazyModuleImp(this) with DontTouch {
    val meip = IO(Input(UInt(3.W)))
    // generate delayed reset for cpu, since chiplink should finish reset
    // to initialize some async modules before accept any requests from cpu
    //val cpu_reset = IO(Flipped(chiselTypeOf(reset)))
    cpu.module.reset := SynchronizerShiftReg(reset.asBool, 10) || reset.asBool

    // connect chiplink slave interface to crossbar
    (chipMaster.slave zip chiplinkNode.in) foreach { case (io, (bundle, _)) => io <> bundle }

    // connect chiplink dma interface to cpu
    cpu.module.slave <> chipMaster.master_mem
    cpu.module.meip := meip

    // connect interrupt signal to cpu
    val intr_from_chipSlave = IO(Input(Bool()))
    // cpu.module.interrupt := intr_from_chipSlave

    // expose chiplink fpga I/O interface as ports
    val fpga_io = IO(chiselTypeOf(chipMaster.module.fpga_io))
    fpga_io <> chipMaster.module.fpga_io

  }
}

class ysyxSoCFPGA(implicit p: Parameters) extends ChipLinkSlave

class FPGATop(implicit p: Parameters) extends LazyModule 
{
  val asic = LazyModule(new ysyxSoCASIC)
  ElaborationArtefacts.add("graphml", graphML)

  override lazy val module = new LazyModuleImp(this) with DontTouch {
    val clockFPGA = IO(Input(Clock()))
    val resetFPGA = IO(Input(Bool()))
    val meip = IO(Input(UInt(3.W)))
    val fpga = withClockAndReset(clockFPGA, resetFPGA) {
      LazyModule(new ysyxSoCFPGA)
    } 
    val masic = asic.module
    val mfpga = withClockAndReset(clockFPGA, resetFPGA) {
      Module(fpga.module)
    } 
    // val mfpga = Module(fpga.module)
    mfpga.dontTouchPorts()
    masic.dontTouchPorts()

    masic.fpga_io.b2c <> mfpga.fpga_io.c2b
    mfpga.fpga_io.b2c <> masic.fpga_io.c2b
    masic.meip := meip

    val io_slave = IO(Flipped(AXI4Bundle(CPUAXI4BundleParameters())))
    val io_master_mem = IO(AXI4Bundle(CPUAXI4BundleParameters()))
    val io_master_mmio = IO(AXI4Bundle(CPUAXI4BundleParameters()))

    // Connect AXI signals
    io_master_mem <> fpga.master_mem.head
    io_master_mmio <> fpga.master_mmio.head
    fpga.slave.head <> io_slave

    // TODO
    masic.intr_from_chipSlave := false.B
  }
}

// ysyxSoC For Simulation
class ysyxSoCFull(implicit p: Parameters) extends LazyModule {
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
  val apbxbar = LazyModule(new APBFanout).node
  val luart = LazyModule(new APBUart16550(AddressSet.misaligned(0x10000000, 0x1000)))
  val lspi  = LazyModule(new APBSPI(
    AddressSet.misaligned(0x10001000, 0x1000) ++    // SPI controller
    AddressSet.misaligned(0x30000000, 0x10000000)   // XIP flash
  ))
  List(lspi.node, luart.node).map(_ := apbxbar)
  apbxbar := AXI4ToAPB() := xbar
  val simMem = AXI4RAM(address = AddressSet.misaligned(ChipLinkParam.mem.base, ChipLinkParam.mem.mask + 1).head, beatBytes =  8)
  simMem := xbar
  lazy val module = new LazyModuleImp(this) with DontTouch {
    val dut = ldut.module
    dut.clockFPGA := clock
    dut.resetFPGA := reset.asBool()
    dut.meip := 0.U
    dut.dontTouchPorts()
    val (mem, _) = memNode.out(0)
    val (mmio, _) = mmioNode.out(0)
    // Connect AXI signals
    mem <> dut.io_master_mem
    mmio <> dut.io_master_mmio
    // Don't connect Slaves
    dut.io_slave.tieoff()
    // UART and SPI
    val spiFlash = Module(new spiFlash)
    spiFlash.io <> lspi.module.spi_bundle
    luart.module.uart.rx := false.B
  }
}
