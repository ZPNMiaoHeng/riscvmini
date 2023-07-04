// See LICENSE for license details.

package mini

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import junctions._

// class CacheReq(addrWidth: Int, dataWidth: Int) extends Bundle {
//   val addr = UInt(addrWidth.W)
//   val data = UInt(dataWidth.W)
//   val mask = UInt((dataWidth / 8).W)
// }

// class CacheResp(dataWidth: Int) extends Bundle {
//   val data = UInt(dataWidth.W)
// }

// class CacheIO(addrWidth: Int, dataWidth: Int) extends Bundle {
//   val abort = Input(Bool())
//   val req = Flipped(Valid(new CacheReq(addrWidth, dataWidth)))    //NOTE(MH):Valid
//   val resp = Valid(new CacheResp(dataWidth))
// }

// class CacheModuleIO(nastiParams: NastiBundleParameters, addrWidth: Int, dataWidth: Int) extends Bundle {
//   val cpu = new CacheIO(addrWidth, dataWidth)
//   val nasti = new NastiBundle(nastiParams)
// }

// case class CacheConfig(nWays: Int, nSets: Int, blockBytes: Int)

class UartModuleIO(nastiParams: NastiBundleParameters, addrWidth: Int, dataWidth: Int) extends Bundle {
  val cpu = new CacheIO(addrWidth, dataWidth)
  val nasti = new NastiBundle(nastiParams)
}

// object UartRState extends ChiselEnum {
//   val sIdle, s, sWriteCache, sWriteBack, sWriteAck, sRefillReady, sRefill = Value
// }

// object UartWState extends ChiselEnum {
//   val sIdle, sReadCache, sWriteCache, sWriteBack, sWriteAck, sRefillReady, sRefill = Value
// }

object UartState extends ChiselEnum {
  val sIdle, sReadAddr, sReadData, sWriteAddr, sWriteData, sWriteBack = Value
}

// normal transform
class Uart(val nasti: NastiBundleParameters, val xlen: Int) extends Module {

    val io = IO(new UartModuleIO(nasti, addrWidth = xlen, dataWidth = xlen))

    // uart state
    import UartState._
    val state = RegInit(sIdle)

    val is_idle = state === sIdle
    val is_read_addr = state === sReadAddr
    val is_read_data = state === sReadData
    val is_write_addr = state === sWriteAddr
    val is_write_data = state === sWriteData
    val is_write_back = state === sWriteBack

      // Read Mux
    io.cpu.resp.bits.data := Fill(32.U, !io.cpu.req.bits.mask.orR) & io.nasti.r.data
    io.cpu.resp.valid := is_idle || io.nasti.r.fire || io.nasti.b.fire 

    // transform 32-byte data
    io.nasti.ar.bits := NastiAddressBundle(nasti)(
        0.U,                          // id
        io.cpu.req.bits.addr,         // addr
        2.U,                          // size
        0.U                           // len
    )
    io.nasti.ar.valid := false.B
    io.nasti.r.read := is_read_data
    

    io.nasti.aw.bits := NastiAddressBundle(nasti)(
        0.U,
        io.cpu.req.bits.addr,
        2.U,
        0.U
    )
    io.nasti.aw.valid := false.B
    io.nasti.w.bits := NastiAddressBundle(nasti)(
        io.cpu.req.bits.data,
        io.cpu.req.bits.mask//,
        //0.U
    )
    io.nasti.w.valid := false.B
    io.nasti.b.ready := is_write_back

    // FSM
    switch(state) {
        is(sIdle) {
            when(io.cpu.req.valid) {
                state := Mux(io.cpu.req.bits.mask.orR, sWriteAddr, sReadAddr)
            }
        }
        is(sReadAddr) {
            io.nasti.ar.valid := true.B
            when(io.nasti.ar.fire) {
                state := sReadData
            }
        }
        is(sReadData) {
            when(io.nasti.r.fire) {
                state := sIdle
            }
        }

        is(sWriteAddr) {
            io.nasti.aw.valid := true.B
            when(io.nasti.aw.fire) {
                state := sWriteData
            }
        }

        is(sWriteData) {
            io.nasti.w.valid := true.B
            when(io.nasti.w.fire) {
                state := sWriteBack
            }
        }

        is(sWriteBack) {
            when(io.nasti.b.fire) {
                state := sIdle
            }
        }
    }


}