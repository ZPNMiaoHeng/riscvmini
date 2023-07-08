// See LICENSE for license details.

package mini

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import junctions._

class MemArbiterIO(params: NastiBundleParameters) extends Bundle {
  val icache = Flipped(new NastiBundle(params))
  val dcache = Flipped(new NastiBundle(params))
  val uart = Flipped(new NastiBundle(params))
  val nasti = new NastiBundle(params)
}

object MemArbiterState extends ChiselEnum {
  val sIdle, sICacheRead, sDCacheRead, sDCacheWrite, sDCacheAck, sUartRead, sUartWrite, sUartAck = Value
}

class MemArbiter(params: NastiBundleParameters) extends Module {
  val io = IO(new MemArbiterIO(params))

  import MemArbiterState._
  val state = RegInit(sIdle)

  // Write Address
  // io.nasti.aw.bits := io.dcache.aw.bits
  io.nasti.aw.bits := NastiAddressBundle(params)(
    Mux(io.dcache.aw.valid, io.dcache.aw.bits.id,   io.uart.aw.bits.id),
    Mux(io.dcache.aw.valid, io.dcache.aw.bits.addr, io.uart.aw.bits.addr),
    Mux(io.dcache.aw.valid, io.dcache.aw.bits.size, io.uart.aw.bits.size),
    Mux(io.dcache.aw.valid, io.dcache.aw.bits.len,  io.uart.aw.bits.len)
  )

  io.nasti.aw.valid := (io.dcache.aw.valid || io.uart.aw.valid) && state === sIdle
  io.dcache.aw.ready := io.nasti.aw.ready && state === sIdle
  io.uart.aw.ready := io.nasti.aw.ready && state === sIdle
  io.icache.aw := DontCare

  // Write Data
  // io.nasti.w.bits := io.dcache.w.bits
  io.nasti.w.bits := NastiWriteDataBundle(params)(
    // Mux(io.dcache.w.valid, io.dcache.w.bits.id,   io.uart.w.bits.id),
    Mux(io.dcache.w.valid, io.dcache.w.bits.data, io.uart.w.bits.data),
    Some(Mux(io.dcache.w.valid, io.dcache.w.bits.strb, io.uart.w.bits.strb)),
    Mux(io.dcache.w.valid, io.dcache.w.bits.last,  io.uart.w.bits.last)
  )


  io.nasti.w.valid := (io.dcache.w.valid && state === sDCacheWrite) || (io.uart.w.valid && state === sUartWrite )
  io.dcache.w.ready := io.nasti.w.ready && state === sDCacheWrite
  io.uart.w.ready := io.nasti.w.ready && state === sUartWrite
  io.icache.w := DontCare

  // Write Ack
  io.dcache.b.bits := io.nasti.b.bits
  io.uart.b.bits := io.nasti.b.bits
  io.dcache.b.valid := io.nasti.b.valid && state === sDCacheAck
  io.uart.b.valid := io.nasti.b.valid && state === sUartAck
  io.nasti.b.ready := (io.dcache.b.ready && state === sDCacheAck) || (io.uart.b.ready && state === sUartAck) 
  io.icache.b := DontCare

  // Read Address
  io.nasti.ar.bits := NastiAddressBundle(params)(
    Mux(io.dcache.ar.valid, io.dcache.ar.bits.id  ,Mux(io.icache.ar.valid, io.icache.ar.bits.id  ,io.uart.ar.bits.id)),
    Mux(io.dcache.ar.valid, io.dcache.ar.bits.addr,Mux(io.icache.ar.valid, io.icache.ar.bits.addr,io.uart.ar.bits.addr)),
    Mux(io.dcache.ar.valid, io.dcache.ar.bits.size,Mux(io.icache.ar.valid, io.icache.ar.bits.size,io.uart.ar.bits.size)),
    Mux(io.dcache.ar.valid, io.dcache.ar.bits.len ,Mux(io.icache.ar.valid, io.icache.ar.bits.len ,io.uart.ar.bits.len))
  )
  io.nasti.ar.valid := (io.icache.ar.valid || io.dcache.ar.valid || io.uart.ar.valid) &&
    !io.nasti.aw.valid && state === sIdle
  io.dcache.ar.ready := io.nasti.ar.ready && !io.nasti.aw.valid && state === sIdle
  io.icache.ar.ready := io.dcache.ar.ready && !io.dcache.ar.valid
  io.uart.ar.ready := io.icache.ar.ready && !io.icache.ar.valid

  // Read Data
  io.icache.r.bits := io.nasti.r.bits
  io.dcache.r.bits := io.nasti.r.bits
  io.uart.r.bits := io.nasti.r.bits
  io.icache.r.valid := io.nasti.r.valid && state === sICacheRead
  io.dcache.r.valid := io.nasti.r.valid && state === sDCacheRead
  io.uart.r.valid := io.nasti.r.valid && state === sUartRead
  io.nasti.r.ready := io.icache.r.ready && state === sICacheRead ||
    io.dcache.r.ready && state === sDCacheRead ||
    io.uart.r.ready && state === sUartRead

  switch(state) {
    is(sIdle) {
      when(io.dcache.aw.fire) {
        state := sDCacheWrite
      }.elsewhen(io.dcache.ar.fire) {
        state := sDCacheRead
      }.elsewhen(io.icache.ar.fire) {
        state := sICacheRead
      }.elsewhen(io.uart.aw.fire) {
        state := sUartWrite
      }.elsewhen(io.uart.ar.fire) {
        state := sUartRead
      }
    }
    is(sICacheRead) {
      when(io.nasti.r.fire && io.nasti.r.bits.last) {
        state := sIdle
      }
    }
    is(sDCacheRead) {
      when(io.nasti.r.fire && io.nasti.r.bits.last) {
        state := sIdle
      }
    }
    is(sDCacheWrite) {
      when(io.dcache.w.fire && io.dcache.w.bits.last) {
        state := sDCacheAck
      }
    }
    is(sDCacheAck) {
      when(io.nasti.b.fire) {
        state := sIdle
      }
    }
    is(sUartRead) {
      when(io.nasti.r.fire && io.nasti.r.bits.last) {
        state := sIdle
      }
    }
    is(sUartWrite){
      when(io.uart.w.fire && io.uart.w.bits.last) {
        state := sUartAck
      }
    }
    is(sUartAck) {
      when(io.nasti.b.fire) {
        state := sIdle
      }
    }
  }
}

class TileIO(xlen: Int, nastiParams: NastiBundleParameters) extends Bundle {
  val host = new HostIO(xlen)
  val nasti = new NastiBundle(nastiParams)
}

object Tile {
  def apply(config: Config): Tile = new Tile(config.core, config.nasti, config.cache)
}

class Tile(val coreParams: CoreConfig, val nastiParams: NastiBundleParameters, val cacheParams: CacheConfig)
    extends Module {
  val io = IO(new TileIO(coreParams.xlen, nastiParams))
  val core = Module(new Core(coreParams))
  val icache = Module(new Cache(cacheParams, nastiParams, coreParams.xlen))
  val dcache = Module(new Cache(cacheParams, nastiParams, coreParams.xlen))
  val uart = Module(new Uart(nastiParams, coreParams.xlen))
  val arb = Module(new MemArbiter(nastiParams))

  io.host <> core.io.host
  core.io.icache <> icache.io.cpu
  core.io.dcache <> dcache.io.cpu
  core.io.uart <> uart.io.cpu
  arb.io.icache <> icache.io.nasti
  arb.io.dcache <> dcache.io.nasti
  arb.io.uart <> uart.io.nasti
  io.nasti <> arb.io.nasti
}
