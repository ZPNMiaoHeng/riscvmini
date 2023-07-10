// See LICENSE for license details.

package mini

import chisel3._
import chisel3.util.Valid

case class CoreConfig(
  xlen:       Int,
  makeAlu:    Int => Alu = new AluSimple(_),
  makeBrCond: Int => BrCond = new BrCondSimple(_),
  makeImmGen: Int => ImmGen = new ImmGenWire(_))

class HostIO(xlen: Int) extends Bundle {            //STUB -  csr 
  val fromhost = Flipped(Valid(UInt(xlen.W)))
  val tohost = Output(UInt(xlen.W))
}

class CoreIO(xlen: Int) extends Bundle {
  val host = new HostIO(xlen)
  val icache = Flipped(new CacheIO(xlen, xlen))
  val dcache = Flipped(new CacheIO(xlen, xlen))
  val iaxi2apb = Flipped(new CacheIO(xlen, xlen))   // FLASH
  val daxi2apb = Flipped(new CacheIO(xlen, xlen))   // FLASH
  val uart = Flipped(new CacheIO(xlen, xlen))
}

class Core(val conf: CoreConfig) extends Module {
  val io = IO(new CoreIO(conf.xlen))
  val dpath = Module(new Datapath(conf))
  val ctrl = Module(new Control)

  io.host <> dpath.io.host
  dpath.io.icache <> io.icache
  dpath.io.dcache <> io.dcache
  dpath.io.iaxi2apb <> io.iaxi2apb
  dpath.io.daxi2apb <> io.daxi2apb
  dpath.io.uart <> io.uart
  dpath.io.ctrl <> ctrl.io
}
