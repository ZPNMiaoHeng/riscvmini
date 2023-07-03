// See LICENSE for license details.

package mini

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import junctions._

class CacheReq(addrWidth: Int, dataWidth: Int) extends Bundle {
  val addr = UInt(addrWidth.W)
  val data = UInt(dataWidth.W)
  val mask = UInt((dataWidth / 8).W)
}

class CacheResp(dataWidth: Int) extends Bundle {
  val data = UInt(dataWidth.W)
}

class CacheIO(addrWidth: Int, dataWidth: Int) extends Bundle {
  val abort = Input(Bool())
  val req = Flipped(Valid(new CacheReq(addrWidth, dataWidth)))    //NOTE(MH):Valid
  val resp = Valid(new CacheResp(dataWidth))
}

class CacheModuleIO(nastiParams: NastiBundleParameters, addrWidth: Int, dataWidth: Int) extends Bundle {
  val cpu = new CacheIO(addrWidth, dataWidth)
  val nasti = new NastiBundle(nastiParams)
}

case class CacheConfig(nWays: Int, nSets: Int, blockBytes: Int)

class MetaData(tagLength: Int) extends Bundle {                        //NOTE - 通过类实现，在后面直接调用
  val tag = UInt(tagLength.W)
}

object CacheState extends ChiselEnum {
  val sIdle, sReadCache, sWriteCache, sWriteBack, sWriteAck, sRefillReady, sRefill = Value
}

class Cache(val p: CacheConfig, val nasti: NastiBundleParameters, val xlen: Int) extends Module {
  // local parameters
  val nSets = p.nSets                        // 256
  val bBytes = p.blockBytes                  // 16
  val bBits = bBytes << 3                    // 128
  val blen = log2Ceil(bBytes)                // 4
  val slen = log2Ceil(nSets)                 // 8
  val tlen = xlen - (slen + blen)            // 20
  val nWords = bBits / xlen                  // 4
  val wBytes = xlen / 8                      // 4
  val byteOffsetBits = log2Ceil(wBytes)      // 2
  val dataBeats = bBits / nasti.dataBits     // 128/64 = 2  //NOTE - MH : ysyxSOC only support brust_size 0b010

  val io = IO(new CacheModuleIO(nasti, addrWidth = xlen, dataWidth = xlen))

  // cache states
  import CacheState._
  val state = RegInit(sIdle)
  // memory
  val v = RegInit(0.U(nSets.W))
  val d = RegInit(0.U(nSets.W))
  val metaMem = SyncReadMem(nSets, new MetaData(tlen))                           // 256 * 20 - tag
  val dataMem = Seq.fill(nWords)(SyncReadMem(nSets, Vec(wBytes, UInt(8.W))))     // 4 * 256 * 4 * 8 ： cache按照字存储

  val addr_reg = Reg(chiselTypeOf(io.cpu.req.bits.addr))
  val cpu_data = Reg(chiselTypeOf(io.cpu.req.bits.data))
  val cpu_mask = Reg(chiselTypeOf(io.cpu.req.bits.mask))

  // Counters
  require(dataBeats > 0)
  val (read_count, read_wrap_out) = Counter(io.nasti.r.fire, dataBeats)     // FIXME: 突发传输统计：64 * 2；size=0b011, brust=0b01-INCR, LEN=1
  val (write_count, write_wrap_out) = Counter(io.nasti.w.fire, dataBeats)   // NOTE

  val is_idle = state === sIdle
  val is_read = state === sReadCache
  val is_write = state === sWriteCache
  val is_alloc = state === sRefill && read_wrap_out
  val is_alloc_reg = RegNext(is_alloc)

  val hit = Wire(Bool())
  val wen = is_write && (hit || is_alloc_reg) && !io.cpu.abort || is_alloc
  val ren = !wen && (is_idle || is_read) && io.cpu.req.valid                //NOTE - wen > ren
  val ren_reg = RegNext(ren)

  val addr = io.cpu.req.bits.addr
  val idx = addr(slen + blen - 1, blen)               // addr[11, 4]
  // NOTE - why reg
  val tag_reg = addr_reg(xlen - 1, slen + blen)       // [19:0] = addr [31. 12] 
  val idx_reg = addr_reg(slen + blen - 1, blen)       // [7:0]  = addr [11, 4]
  val off_reg = addr_reg(blen - 1, byteOffsetBits)    // [1:0]  = addr [3, 2]

  val rmeta = metaMem.read(idx, ren)                                                    //NOTE - 传入 idx 和ren ，输出tag
  val rdata = Cat((dataMem.map(_.read(idx, ren).asUInt)).reverse)  //FIXME - 
  val rdata_buf = RegEnable(rdata, ren_reg)
  val refill_buf = Reg(Vec(dataBeats, UInt(nasti.dataBits.W)))
  val read = Mux(is_alloc_reg, refill_buf.asUInt, Mux(ren_reg, rdata, rdata_buf))

  hit := v(idx_reg) && rmeta.tag === tag_reg         //ANCHOR - 为啥 v中使用idx_reg呢

  // Read Mux
  io.cpu.resp.bits.data := VecInit.tabulate(nWords)(i => read((i + 1) * xlen - 1, i * xlen))(off_reg) //FIXME - 
  io.cpu.resp.valid := is_idle || is_read && hit || is_alloc_reg && !cpu_mask.orR    //NOTE - 

  when(io.cpu.resp.valid) {
    addr_reg := addr
    cpu_data := io.cpu.req.bits.data
    cpu_mask := io.cpu.req.bits.mask
  }

  val wmeta = Wire(new MetaData(tlen))
  wmeta.tag := tag_reg

  val wmask = Mux(!is_alloc, (cpu_mask << Cat(off_reg, 0.U(byteOffsetBits.W))).zext, (-1).S)
  val wdata = Mux(
    !is_alloc,
    Fill(nWords, cpu_data),
    if (refill_buf.size == 1) io.nasti.r.bits.data
    else Cat(io.nasti.r.bits.data, Cat(refill_buf.init.reverse))
  )
  when(wen) {
    v := v.bitSet(idx_reg, true.B)
    d := d.bitSet(idx_reg, !is_alloc)
    when(is_alloc) {
      metaMem.write(idx_reg, wmeta)
    }
    dataMem.zipWithIndex.foreach {
      case (mem, i) =>
        val data = VecInit.tabulate(wBytes)(k => wdata(i * xlen + (k + 1) * 8 - 1, i * xlen + k * 8))
        mem.write(idx_reg, data, wmask((i + 1) * wBytes - 1, i * wBytes).asBools())
        mem.suggestName(s"dataMem_${i}")
    }
  }

  io.nasti.ar.bits := NastiAddressBundle(nasti)(
    0.U,                                              //NOTE - ar_id
    (Cat(tag_reg, idx_reg) << blen.U).asUInt,         //NOTE - ar_addr: uart no alian
    log2Up(nasti.dataBits / 8).U,
    // 3.U, //log2Up(nasti.dataBits / 8).U,              //NOTE - ar_size log2UP(64/8)=3, but ysyxSOC only 2
    (dataBeats - 1).U                                 //NOTE - ar_bits                 //TODO - modify 4
  )
  io.nasti.ar.valid := false.B
  // read data
  io.nasti.r.ready := state === sRefill
  when(io.nasti.r.fire) {
    refill_buf(read_count) := io.nasti.r.bits.data
  }

  // write addr
  io.nasti.aw.bits := NastiAddressBundle(nasti)(
    0.U,
    (Cat(rmeta.tag, idx_reg) << blen.U).asUInt,
    // log2Up(nasti.dataBits / 8).U,
    2.U, ////NOTE - ar_size log2UP(64/8)=3, but ysyxSOC only 2
    (dataBeats - 1).U
  )
  io.nasti.aw.valid := false.B
  // write data
  io.nasti.w.bits := NastiWriteDataBundle(nasti)(
    VecInit.tabulate(dataBeats)(i => read((i + 1) * nasti.dataBits - 1, i * nasti.dataBits))(write_count),
    None,
    write_wrap_out
  )
  io.nasti.w.valid := false.B
  // write resp
  io.nasti.b.ready := false.B

  // Cache FSM
  val is_dirty = v(idx_reg) && d(idx_reg)
  switch(state) {
    is(sIdle) {
      when(io.cpu.req.valid) {
        state := Mux(io.cpu.req.bits.mask.orR, sWriteCache, sReadCache)
      }
    }
    is(sReadCache) {
      when(hit) {
        when(io.cpu.req.valid) {
          state := Mux(io.cpu.req.bits.mask.orR, sWriteCache, sReadCache)
        }.otherwise {
          state := sIdle
        }
      }.otherwise {
        io.nasti.aw.valid := is_dirty
        io.nasti.ar.valid := !is_dirty
        when(io.nasti.aw.fire) {
          state := sWriteBack
        }.elsewhen(io.nasti.ar.fire) {
          state := sRefill
        }
      }
    }
    is(sWriteCache) {
      when(hit || is_alloc_reg || io.cpu.abort) {
        state := sIdle
      }.otherwise {
        io.nasti.aw.valid := is_dirty
        io.nasti.ar.valid := !is_dirty
        when(io.nasti.aw.fire) {
          state := sWriteBack
        }.elsewhen(io.nasti.ar.fire) {
          state := sRefill
        }
      }
    }
    is(sWriteBack) {
      io.nasti.w.valid := true.B
      when(write_wrap_out) {
        state := sWriteAck
      }
    }
    is(sWriteAck) {
      io.nasti.b.ready := true.B
      when(io.nasti.b.fire) {
        state := sRefillReady
      }
    }
    is(sRefillReady) {
      io.nasti.ar.valid := true.B
      when(io.nasti.ar.fire) {
        state := sRefill
      }
    }
    is(sRefill) {
      when(read_wrap_out) {
        state := Mux(cpu_mask.orR, sWriteCache, sIdle)
      }
    }
  }
}
