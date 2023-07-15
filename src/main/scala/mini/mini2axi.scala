// See LICENSE for license details.

package mini

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import junctions._

class Mini2axiModuleIO(nastiParams: NastiBundleParameters, addrWidth: Int, dataWidth: Int) extends Bundle {
    val cpu = new CacheIO(addrWidth, dataWidth)
    val nasti = new NastiBundle(nastiParams)
}
object Mini2axiState extends ChiselEnum {
    val sIdle, sReadAddr, sReadData, sWriteAddr, sWriteData, sWriteBack = Value
}

// normal transform
class Mini2axi(val nasti: NastiBundleParameters, val xlen: Int) extends Module {

    val io = IO(new Mini2axiModuleIO(nasti, addrWidth = xlen, dataWidth = xlen))

    // uart state
    import Mini2axiState._
    val state = RegInit(sIdle)

    val is_idle = state === sIdle
    val is_read_addr = state === sReadAddr
    val is_read_data = state === sReadData
    val is_write_addr = state === sWriteAddr
    val is_write_data = state === sWriteData
    val is_write_back = state === sWriteBack

    val reg_mask = RegInit(0.U(4.W))
    val reg_addr = RegInit(0.U(xlen.W))
    val reg_data = RegInit(0.U(xlen.W))

      // Read Mux
    io.cpu.resp.bits.data := io.nasti.r.bits.data
    // io.cpu.resp.bits.data := Mux(io.cpu.req.bits.mask.orR,  Instructions.NOP, io.nasti.r.bits.data)
    // io.cpu.resp.bits.data := Fill(32, !io.cpu.req.bits.mask.orR) & io.nasti.r.bits.data
    io.cpu.resp.valid := is_idle || io.nasti.r.fire || io.nasti.b.fire 
    // io.cpu.resp.valid := io.nasti.r.fire || io.nasti.b.fire

    // transform 32-byte data
    io.nasti.ar.bits := NastiAddressBundle(nasti)(
        0.U,                          // id
        io.cpu.req.bits.addr,         // addr
        // reg_addr,
        2.U,                          // size
        0.U                           // len
    )
    io.nasti.ar.valid := false.B
    io.nasti.r.ready := is_read_data
    

    io.nasti.aw.bits := NastiAddressBundle(nasti)(
        0.U,
        // io.cpu.req.bits.addr,
        // RegNext(io.cpu.req.bits.addr),
        reg_addr,
        2.U,
        0.U
    )
    io.nasti.aw.valid := false.B
    // io.nasti.w.bits := NastiWriteDataBundle(nasti)(
        // io.cpu.req.bits.data,
        // Some(io.cpu.req.bits.mask)//,
        // 0.U
    // )
    io.nasti.w.bits := NastiWriteDataBundle(nasti)(
        reg_data ## reg_data,
        Some(Mux(io.nasti.aw.bits.addr(2, 2) === 1.U, reg_mask << 4.U, reg_mask))    //FIXME - apb write data mask angin 
        //0.U
    )
    io.nasti.w.valid := false.B
    io.nasti.b.ready := is_write_back

    // FSM
    switch(state) {
        is(sIdle) {
            when(io.cpu.req.valid) {
                reg_addr := io.cpu.req.bits.addr
                reg_data := io.cpu.req.bits.data
                reg_mask := io.cpu.req.bits.mask
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
                when(io.cpu.req.valid) {
                    reg_data := io.cpu.req.bits.data
                    reg_mask := io.cpu.req.bits.mask
                    state := Mux(io.cpu.req.bits.mask.orR, sWriteAddr, sReadAddr)
                } .otherwise {
                    state := sIdle
                }
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