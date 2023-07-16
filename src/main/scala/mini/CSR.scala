// See LICENSE for license details.

package mini

import chisel3._
import chisel3.util._

object CSR {
  val N = 0.U(3.W)
  val W = 1.U(3.W)
  val S = 2.U(3.W)
  val C = 3.U(3.W)
  val P = 4.U(3.W)

  // Supports machine & user modes
  val PRV_U = 0x0.U(2.W)
  val PRV_M = 0x3.U(2.W)

  // User-level CSR addrs
  val cycle = 0xc00.U(12.W)
  val time = 0xc01.U(12.W)
  val instret = 0xc02.U(12.W)
  val cycleh = 0xc80.U(12.W)
  val timeh = 0xc81.U(12.W)
  val instreth = 0xc82.U(12.W)

  // Supervisor-level CSR addrs  //??? not find
  val cyclew = 0x900.U(12.W)
  val timew = 0x901.U(12.W)
  val instretw = 0x902.U(12.W)
  val cyclehw = 0x980.U(12.W)
  val timehw = 0x981.U(12.W)
  val instrethw = 0x982.U(12.W)

  // Machine-level CSR addrs
  // Machine Information Registers
  val mcpuid = 0xf00.U(12.W)    //FIXME - no csr
  val mimpid = 0xf13.U(12.W)
  val mhartid = 0xf14.U(12.W)
  // Machine Trap Setup
  val mstatus = 0x300.U(12.W)
  val mtvec = 0x305.U(12.W)
  val mtdeleg = 0x302.U(12.W)   //FIXME -  no scr medeleg
  val mie = 0x304.U(12.W)
  val mtimecmp = 0x321.U(12.W)
  // Machine Timers and Counters
  val mtime = 0x701.U(12.W)
  val mtimeh = 0x741.U(12.W)
  // Machine Trap Handling
  val mscratch = 0x340.U(12.W)
  val mepc = 0x341.U(12.W)
  val mcause = 0x342.U(12.W)
  val mbadaddr = 0x343.U(12.W)   //NOTE - mtval
  val mip = 0x344.U(12.W)
  // Machine HITF ： Hardware Implementation-Defined Trap，map Standard read/write
  val mtohost = 0x780.U(12.W)
  val mfromhost = 0x781.U(12.W)

  val regs = List(
    cycle,
    time,
    instret,
    cycleh,
    timeh,
    instreth,
    cyclew,
    timew,
    instretw,
    cyclehw,
    timehw,
    instrethw,
    mcpuid,
    mimpid,
    mhartid,
    mtvec,
    mtdeleg,
    mie,
    mtimecmp,
    mtime,
    mtimeh,
    mscratch,
    mepc,
    mcause,
    mbadaddr,
    mip,
    mtohost,
    mfromhost,
    mstatus
  )
}

object Cause {
  val InstAddrMisaligned = 0x0.U
  val IllegalInst = 0x2.U
  val Breakpoint = 0x3.U
  val LoadAddrMisaligned = 0x4.U
  val StoreAddrMisaligned = 0x6.U
  val Ecall = 0x8.U

  val MachineTimerInterrupt = 0x7.U   //NOTE - 0x8000_00007
}

class CSRIO(xlen: Int) extends Bundle {
  val stall = Input(Bool())
  val cmd = Input(UInt(3.W))
  val in = Input(UInt(xlen.W))
  val out = Output(UInt(xlen.W))
  // Excpetion
  val pc = Input(UInt(xlen.W))
  val addr = Input(UInt(xlen.W))
  val inst = Input(UInt(xlen.W))
  val illegal = Input(Bool())
  val st_type = Input(UInt(2.W))
  val ld_type = Input(UInt(3.W))
  val pc_check = Input(Bool())
  val expt = Output(Bool())
  val evec = Output(UInt(xlen.W))   //????
  val epc = Output(UInt(xlen.W))

  // val in_valid = Input(Bool())
  // val in_mtimecmp = Input(UInt(64.W))
  // val out_mtimecmp = Output(UInt(64.W))
  // val mTimerInterrupt = Output(Bool())
  val clint = new CacheIO(xlen, xlen)
  // HTIF
  val host = new HostIO(xlen)
}

class CSR(val xlen: Int) extends Module {
  val io = IO(new CSRIO(xlen))

  val csr_addr = io.inst(31, 20)
  val rs1_addr = io.inst(19, 15)

  // user counters
  val time = RegInit(0.U(xlen.W))
  val timeh = RegInit(0.U(xlen.W))
  val cycle = RegInit(0.U(xlen.W))
  val cycleh = RegInit(0.U(xlen.W))
  val instret = RegInit(0.U(xlen.W))
  val instreth = RegInit(0.U(xlen.W))

  val mcpuid = Cat(
    0.U(2.W) /* RV32I */,
    0.U((xlen - 28).W),
    (1 << ('I' - 'A') /* Base ISA */ |
      1 << ('U' - 'A') /* User Mode */ ).U(26.W)
  )
  val mimpid = 0.U(xlen.W) // not implemented
  val mhartid = 0.U(xlen.W) // only one hart

  val MIE = RegInit(false.B)   //TODO - 启用中断
  val MPIE = RegInit(false.B)   //TODO - trap 之前的MIE
  val MPP = RegInit(CSR.PRV_M)  //TODO - tarp 之前的特权模式
  val WPRI = 0.U(1.W)
  val SIE = 0.U(1.W)
  val SPIE = 0.U(1.W)
  val UBE = 0.U(1.W)
  val SPP = 0.U(1.W)
  val VS = 0.U(2.W)
  val XS = 0.U(2.W)
  val FS = 0.U(2.W)
  val SD = 0.U(1.W)
  val VM = 0.U(5.W)
  val MPRV = false.B

  val mstatus = Cat(SD, 0.U((xlen - 23).W), VM, MPRV, XS, FS, MPP, VS ,SPP, MPIE, UBE, SPIE, WPRI, MIE, WPRI, SIE, WPRI)
  
  val mtvec = RegInit(Const.PC_EVEC.U(xlen.W))
  val mtdeleg = 0x0.U(xlen.W)

  // interrupt registers
  val MTIP = RegInit(false.B)
  val HTIP = false.B
  val STIP = false.B
  val MTIE = RegInit(false.B)
  val HTIE = false.B
  val STIE = false.B
  val MSIP = RegInit(false.B)
  val HSIP = false.B
  val SSIP = false.B
  val MSIE = RegInit(false.B)   //NOTE - S mode interrupt
  val HSIE = false.B
  val SSIE = false.B
  val mip = Cat(0.U((xlen - 8).W), MTIP, HTIP, STIP, false.B, MSIP, HSIP, SSIP, false.B)
  val mie = Cat(0.U((xlen - 8).W), MTIE, HTIE, STIE, false.B, MSIE, HSIE, SSIE, false.B)

  val mtimecmp = Reg(UInt(xlen.W))
  val mtimecmpH = Reg(UInt(xlen.W))

  val mscratch = Reg(UInt(xlen.W))

  val mepc = Reg(UInt(xlen.W))
  val mcause = Reg(UInt(xlen.W))
  val mbadaddr = Reg(UInt(xlen.W))

  val mtohost = RegInit(0.U(xlen.W))    // control sim
  val mfromhost = Reg(UInt(xlen.W))
  io.host.tohost := mtohost
  when(io.host.fromhost.valid) {
    mfromhost := io.host.fromhost.bits
  }

  val csrFile = Seq(
    BitPat(CSR.cycle) -> cycle,
    BitPat(CSR.time) -> time,
    BitPat(CSR.instret) -> instret,
    BitPat(CSR.cycleh) -> cycleh,
    BitPat(CSR.timeh) -> timeh,
    BitPat(CSR.instreth) -> instreth,
    BitPat(CSR.cyclew) -> cycle,
    BitPat(CSR.timew) -> time,
    BitPat(CSR.instretw) -> instret,
    BitPat(CSR.cyclehw) -> cycleh,
    BitPat(CSR.timehw) -> timeh,
    BitPat(CSR.instrethw) -> instreth,
    BitPat(CSR.mcpuid) -> mcpuid,
    BitPat(CSR.mimpid) -> mimpid,
    BitPat(CSR.mhartid) -> mhartid,
    BitPat(CSR.mtvec) -> mtvec,
    BitPat(CSR.mtdeleg) -> mtdeleg,
    BitPat(CSR.mie) -> mie,
    BitPat(CSR.mtimecmp) -> mtimecmp,
    BitPat(CSR.mtime) -> time,
    BitPat(CSR.mtimeh) -> timeh,
    BitPat(CSR.mscratch) -> mscratch,
    BitPat(CSR.mepc) -> mepc,
    BitPat(CSR.mcause) -> mcause,
    BitPat(CSR.mbadaddr) -> mbadaddr,
    BitPat(CSR.mip) -> mip,
    BitPat(CSR.mtohost) -> mtohost,
    BitPat(CSR.mfromhost) -> mfromhost,
    BitPat(CSR.mstatus) -> mstatus
  )

  io.out := Lookup(csr_addr, 0.U, csrFile).asUInt      // note: csr value

  // val privValid = csr_addr(9, 8) <= PRV   //判断csr_addr的9到8位是否小于等于PRV。其中，PRV表示Mstatus寄存器中的特权位
  val privValid = csr_addr(9, 8) <= MPP
  val privInst = io.cmd === CSR.P
  val isEcall = privInst && !csr_addr(0) && !csr_addr(8)
  val isEbreak = privInst && csr_addr(0) && !csr_addr(8)
  val isEret = privInst && !csr_addr(0) && csr_addr(8)
  val isMret = privInst && !csr_addr(0) && csr_addr(1) && csr_addr(8) && csr_addr(9)
  val csrValid = csrFile.map(_._1 === csr_addr).reduce(_ || _)  //NOTE - 判断csr地址是否有效！ scala用法
  // val csrRO = csr_addr(11, 10).andR || csr_addr === CSR.mtvec || csr_addr === CSR.mtdeleg
  val csrRO = csr_addr(11, 10).andR || csr_addr === CSR.mtdeleg
  val wen = io.cmd === CSR.W || io.cmd(1) && rs1_addr.orR
  val wdata = MuxLookup(
    io.cmd,
    0.U,
    Seq(
      CSR.W -> io.in,
      CSR.S -> (io.out | io.in),
      CSR.C -> (io.out & ~io.in)
    )
  )
  val iaddrInvalid = io.pc_check && io.addr(1)
  val laddrInvalid = MuxLookup(
    io.ld_type,
    false.B,
    Seq(Control.LD_LW -> io.addr(1, 0).orR, Control.LD_LH -> io.addr(0), Control.LD_LHU -> io.addr(0))
  )
  val saddrInvalid =
    MuxLookup(io.st_type, false.B, Seq(Control.ST_SW -> io.addr(1, 0).orR, Control.ST_SH -> io.addr(0)))

  // val mtime = RegInit(0.U(64.W))
  val mtimeLEn = io.clint.req.valid && (io.clint.req.bits.addr === 0x0200_BFF8.U)
  val mtimeHEn = io.clint.req.valid && (io.clint.req.bits.addr === 0x0200_BFFC.U)
  val mtimecmpLEn = io.clint.req.valid && (io.clint.req.bits.addr === 0x0200_4000.U)
  val mtimecmpHEn = io.clint.req.valid && (io.clint.req.bits.addr === 0x0200_4004.U)
  // time := Mux(io.clint.req.mask.orR && mtimeLEn, 
  when(io.clint.req.bits.mask.orR) {
    when(mtimeLEn) {
      time := io.clint.req.bits.data
    }.elsewhen(mtimeHEn) {
      timeh := io.clint.req.bits.data
    }.elsewhen(mtimecmpLEn) {
      mtimecmp := io.clint.req.bits.data
    }.elsewhen(mtimecmpHEn) {
      mtimecmpH := io.clint.req.bits.data
    }
  }
  io.clint.resp.bits.data := Mux(!io.clint.req.bits.mask.orR && io.clint.req.valid, 
    MuxLookup(
      io.clint.req.bits.addr,
      0.U,
      Seq(
        0x0200_BFF8.U -> time,
        0x0200_BFFC.U -> timeh,
        0x0200_4000.U -> mtimecmp,
        0x0200_4004.U -> mtimecmpH
        )
      ),
      0.U)
  io.clint.resp.valid :=  RegNext(io.clint.req.valid) // FIXME

  val mtime = timeh ## time
  val mtimecmpT = mtimecmpH ## mtimecmp
  // when(io.in_valid) {
    // mtimecmp := io.in_mtimecmp
  // }

  val mTimerInterrupt = (mtime > mtimecmpT) && MTIE && MIE

  // io.mTimerInterrupt := mTimerInterrupt
  // io.out_mtimecmp := mtimecmp
  io.expt := io.illegal || iaddrInvalid || laddrInvalid || saddrInvalid ||
    io.cmd(1, 0).orR && (!csrValid || !privValid) || wen && csrRO ||
    (privInst && !privValid) || isEcall || isEbreak || mTimerInterrupt
  io.evec := mtvec // NOTE - why use it  + (PRV << 6)   // mtvec + PRV ## 00_0000 -> low Trap Vector Address=0x1C0
  io.epc := mepc   //NOTE - eret/mret：exit pc

  // Counters
  time := time + 1.U
  when(time.andR) { timeh := timeh + 1.U }
  cycle := cycle + 1.U
  when(cycle.andR) { cycleh := cycleh + 1.U }
  val isInstRet = io.inst =/= Instructions.NOP && (!io.expt || isEcall || isEbreak) && !io.stall
  when(isInstRet) { instret := instret + 1.U }
  when(isInstRet && instret.andR) { instreth := instreth + 1.U }

  when(!io.stall) {
    when(io.expt) {
      mepc := io.pc >> 2 << 2
      mcause := Mux(
        iaddrInvalid,
        Cause.InstAddrMisaligned,
        Mux(
          laddrInvalid,
          Cause.LoadAddrMisaligned,
          Mux(
            saddrInvalid,
            Cause.StoreAddrMisaligned,
            Mux(
              mTimerInterrupt,
              (BigInt(1) << (xlen - 1)).U | Cause.MachineTimerInterrupt, // mcause m intr = 0x8000_0007
              Mux(isEcall, Cause.Ecall + MPP, Mux(isEbreak, Cause.Breakpoint, Cause.IllegalInst))   // Cause.Ecall + PRV = 8 + U(0)/M(3)
            )
          )
        )
      )
      MPIE := MIE
      MIE := false.B
      MPP := CSR.PRV_M
      when(iaddrInvalid || laddrInvalid || saddrInvalid) { mbadaddr := io.addr }
    }.elsewhen(isMret) {
      MIE := MPIE
      MPIE := true.B
      MPP := CSR.PRV_U

    }.elsewhen(wen) {
      when(csr_addr === CSR.mstatus) {
        MIE := wdata(3)
        MPIE := wdata(7)
        MPP := wdata(12, 11)
      }
        .elsewhen(csr_addr === CSR.mip) {
          MTIP := wdata(7)
          MSIP := wdata(3)
        }
        .elsewhen(csr_addr === CSR.mie) {
          MTIE := wdata(7)
          MSIE := wdata(3)
        }
        .elsewhen(csr_addr === CSR.mtime) { time := wdata }
        .elsewhen(csr_addr === CSR.mtimeh) { timeh := wdata }
        .elsewhen(csr_addr === CSR.mtimecmp) { mtimecmp := wdata }
        .elsewhen(csr_addr === CSR.mscratch) { mscratch := wdata }
        .elsewhen(csr_addr === CSR.mepc) { mepc := wdata >> 2.U << 2.U }
        .elsewhen(csr_addr === CSR.mcause) { mcause := wdata & (BigInt(1) << (xlen - 1) | 0xf).U }
        .elsewhen(csr_addr === CSR.mbadaddr) { mbadaddr := wdata }
        .elsewhen(csr_addr === CSR.mtohost) { mtohost := wdata }
        .elsewhen(csr_addr === CSR.mfromhost) { mfromhost := wdata }
        .elsewhen(csr_addr === CSR.cyclew) { cycle := wdata }
        .elsewhen(csr_addr === CSR.timew) { time := wdata }
        .elsewhen(csr_addr === CSR.instretw) { instret := wdata }
        .elsewhen(csr_addr === CSR.cyclehw) { cycleh := wdata }
        .elsewhen(csr_addr === CSR.timehw) { timeh := wdata }
        .elsewhen(csr_addr === CSR.instrethw) { instreth := wdata }
        .elsewhen(csr_addr === CSR.mtvec) { mtvec := wdata >> 2.U << 2.U}  // Direct
    }
  }
}
