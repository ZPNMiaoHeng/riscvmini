module CSR(
  input         clock,
  input         reset,
  input         io_stall,
  input  [2:0]  io_cmd,
  input  [31:0] io_in,
  output [31:0] io_out,
  input  [31:0] io_pc,
  input  [31:0] io_addr,
  input  [31:0] io_inst,
  input         io_illegal,
  input  [1:0]  io_st_type,
  input  [2:0]  io_ld_type,
  input         io_pc_check,
  output        io_expt,
  output [31:0] io_evec,
  output [31:0] io_epc,
  input         io_in_valid,
  input  [63:0] io_in_mtimecmp,
  input         io_host_fromhost_valid,
  input  [31:0] io_host_fromhost_bits,
  output [31:0] io_host_tohost
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
  reg [31:0] _RAND_1;
  reg [31:0] _RAND_2;
  reg [31:0] _RAND_3;
  reg [31:0] _RAND_4;
  reg [31:0] _RAND_5;
  reg [31:0] _RAND_6;
  reg [31:0] _RAND_7;
  reg [31:0] _RAND_8;
  reg [31:0] _RAND_9;
  reg [31:0] _RAND_10;
  reg [31:0] _RAND_11;
  reg [31:0] _RAND_12;
  reg [31:0] _RAND_13;
  reg [31:0] _RAND_14;
  reg [31:0] _RAND_15;
  reg [31:0] _RAND_16;
  reg [31:0] _RAND_17;
  reg [31:0] _RAND_18;
  reg [31:0] _RAND_19;
  reg [31:0] _RAND_20;
  reg [63:0] _RAND_21;
`endif // RANDOMIZE_REG_INIT
  wire [11:0] csr_addr = io_inst[31:20]; // @[CSR.scala 131:25]
  wire [4:0] rs1_addr = io_inst[19:15]; // @[CSR.scala 132:25]
  reg [31:0] time_; // @[CSR.scala 135:21]
  reg [31:0] timeh; // @[CSR.scala 136:22]
  reg [31:0] cycle; // @[CSR.scala 137:22]
  reg [31:0] cycleh; // @[CSR.scala 138:23]
  reg [31:0] instret; // @[CSR.scala 139:24]
  reg [31:0] instreth; // @[CSR.scala 140:25]
  reg  MIE; // @[CSR.scala 170:20]
  reg  MPIE; // @[CSR.scala 171:21]
  reg [1:0] MPP; // @[CSR.scala 172:20]
  wire [32:0] mstatus = {18'h0,2'h0,MPP,3'h0,MPIE,1'h0,2'h0,MIE,1'h0,2'h0}; // @[Cat.scala 31:58]
  reg [31:0] mtvec; // @[CSR.scala 188:22]
  reg  MTIP; // @[CSR.scala 192:21]
  reg  MTIE; // @[CSR.scala 195:21]
  reg  MSIP; // @[CSR.scala 198:21]
  reg  MSIE; // @[CSR.scala 201:21]
  wire [31:0] mip = {24'h0,MTIP,1'h0,2'h0,MSIP,1'h0,2'h0}; // @[Cat.scala 31:58]
  wire [31:0] mie = {24'h0,MTIE,1'h0,2'h0,MSIE,1'h0,2'h0}; // @[Cat.scala 31:58]
  reg [31:0] mtimecmp; // @[CSR.scala 207:21]
  reg [31:0] mscratch; // @[CSR.scala 209:21]
  reg [31:0] mepc; // @[CSR.scala 211:17]
  reg [31:0] mcause; // @[CSR.scala 212:19]
  reg [31:0] mbadaddr; // @[CSR.scala 213:21]
  reg [31:0] mtohost; // @[CSR.scala 215:24]
  reg [31:0] mfromhost; // @[CSR.scala 216:22]
  wire [31:0] _GEN_0 = io_host_fromhost_valid ? io_host_fromhost_bits : mfromhost; // @[CSR.scala 218:32 219:15 216:22]
  wire  _io_out_T_1 = 12'hc00 == csr_addr; // @[Lookup.scala 31:38]
  wire  _io_out_T_3 = 12'hc01 == csr_addr; // @[Lookup.scala 31:38]
  wire  _io_out_T_5 = 12'hc02 == csr_addr; // @[Lookup.scala 31:38]
  wire  _io_out_T_7 = 12'hc80 == csr_addr; // @[Lookup.scala 31:38]
  wire  _io_out_T_9 = 12'hc81 == csr_addr; // @[Lookup.scala 31:38]
  wire  _io_out_T_11 = 12'hc82 == csr_addr; // @[Lookup.scala 31:38]
  wire  _io_out_T_13 = 12'h900 == csr_addr; // @[Lookup.scala 31:38]
  wire  _io_out_T_15 = 12'h901 == csr_addr; // @[Lookup.scala 31:38]
  wire  _io_out_T_17 = 12'h902 == csr_addr; // @[Lookup.scala 31:38]
  wire  _io_out_T_19 = 12'h980 == csr_addr; // @[Lookup.scala 31:38]
  wire  _io_out_T_21 = 12'h981 == csr_addr; // @[Lookup.scala 31:38]
  wire  _io_out_T_23 = 12'h982 == csr_addr; // @[Lookup.scala 31:38]
  wire  _io_out_T_25 = 12'hf00 == csr_addr; // @[Lookup.scala 31:38]
  wire  _io_out_T_27 = 12'hf13 == csr_addr; // @[Lookup.scala 31:38]
  wire  _io_out_T_29 = 12'hf14 == csr_addr; // @[Lookup.scala 31:38]
  wire  _io_out_T_31 = 12'h305 == csr_addr; // @[Lookup.scala 31:38]
  wire  _io_out_T_33 = 12'h302 == csr_addr; // @[Lookup.scala 31:38]
  wire  _io_out_T_35 = 12'h304 == csr_addr; // @[Lookup.scala 31:38]
  wire  _io_out_T_37 = 12'h321 == csr_addr; // @[Lookup.scala 31:38]
  wire  _io_out_T_39 = 12'h701 == csr_addr; // @[Lookup.scala 31:38]
  wire  _io_out_T_41 = 12'h741 == csr_addr; // @[Lookup.scala 31:38]
  wire  _io_out_T_43 = 12'h340 == csr_addr; // @[Lookup.scala 31:38]
  wire  _io_out_T_45 = 12'h341 == csr_addr; // @[Lookup.scala 31:38]
  wire  _io_out_T_47 = 12'h342 == csr_addr; // @[Lookup.scala 31:38]
  wire  _io_out_T_49 = 12'h343 == csr_addr; // @[Lookup.scala 31:38]
  wire  _io_out_T_51 = 12'h344 == csr_addr; // @[Lookup.scala 31:38]
  wire  _io_out_T_53 = 12'h780 == csr_addr; // @[Lookup.scala 31:38]
  wire  _io_out_T_55 = 12'h781 == csr_addr; // @[Lookup.scala 31:38]
  wire  _io_out_T_57 = 12'h300 == csr_addr; // @[Lookup.scala 31:38]
  wire [32:0] _io_out_T_58 = _io_out_T_57 ? mstatus : 33'h0; // @[Lookup.scala 34:39]
  wire [32:0] _io_out_T_59 = _io_out_T_55 ? {{1'd0}, mfromhost} : _io_out_T_58; // @[Lookup.scala 34:39]
  wire [32:0] _io_out_T_60 = _io_out_T_53 ? {{1'd0}, mtohost} : _io_out_T_59; // @[Lookup.scala 34:39]
  wire [32:0] _io_out_T_61 = _io_out_T_51 ? {{1'd0}, mip} : _io_out_T_60; // @[Lookup.scala 34:39]
  wire [32:0] _io_out_T_62 = _io_out_T_49 ? {{1'd0}, mbadaddr} : _io_out_T_61; // @[Lookup.scala 34:39]
  wire [32:0] _io_out_T_63 = _io_out_T_47 ? {{1'd0}, mcause} : _io_out_T_62; // @[Lookup.scala 34:39]
  wire [32:0] _io_out_T_64 = _io_out_T_45 ? {{1'd0}, mepc} : _io_out_T_63; // @[Lookup.scala 34:39]
  wire [32:0] _io_out_T_65 = _io_out_T_43 ? {{1'd0}, mscratch} : _io_out_T_64; // @[Lookup.scala 34:39]
  wire [32:0] _io_out_T_66 = _io_out_T_41 ? {{1'd0}, timeh} : _io_out_T_65; // @[Lookup.scala 34:39]
  wire [32:0] _io_out_T_67 = _io_out_T_39 ? {{1'd0}, time_} : _io_out_T_66; // @[Lookup.scala 34:39]
  wire [32:0] _io_out_T_68 = _io_out_T_37 ? {{1'd0}, mtimecmp} : _io_out_T_67; // @[Lookup.scala 34:39]
  wire [32:0] _io_out_T_69 = _io_out_T_35 ? {{1'd0}, mie} : _io_out_T_68; // @[Lookup.scala 34:39]
  wire [32:0] _io_out_T_70 = _io_out_T_33 ? 33'h0 : _io_out_T_69; // @[Lookup.scala 34:39]
  wire [32:0] _io_out_T_71 = _io_out_T_31 ? {{1'd0}, mtvec} : _io_out_T_70; // @[Lookup.scala 34:39]
  wire [32:0] _io_out_T_72 = _io_out_T_29 ? 33'h0 : _io_out_T_71; // @[Lookup.scala 34:39]
  wire [32:0] _io_out_T_73 = _io_out_T_27 ? 33'h0 : _io_out_T_72; // @[Lookup.scala 34:39]
  wire [32:0] _io_out_T_74 = _io_out_T_25 ? 33'h100100 : _io_out_T_73; // @[Lookup.scala 34:39]
  wire [32:0] _io_out_T_75 = _io_out_T_23 ? {{1'd0}, instreth} : _io_out_T_74; // @[Lookup.scala 34:39]
  wire [32:0] _io_out_T_76 = _io_out_T_21 ? {{1'd0}, timeh} : _io_out_T_75; // @[Lookup.scala 34:39]
  wire [32:0] _io_out_T_77 = _io_out_T_19 ? {{1'd0}, cycleh} : _io_out_T_76; // @[Lookup.scala 34:39]
  wire [32:0] _io_out_T_78 = _io_out_T_17 ? {{1'd0}, instret} : _io_out_T_77; // @[Lookup.scala 34:39]
  wire [32:0] _io_out_T_79 = _io_out_T_15 ? {{1'd0}, time_} : _io_out_T_78; // @[Lookup.scala 34:39]
  wire [32:0] _io_out_T_80 = _io_out_T_13 ? {{1'd0}, cycle} : _io_out_T_79; // @[Lookup.scala 34:39]
  wire [32:0] _io_out_T_81 = _io_out_T_11 ? {{1'd0}, instreth} : _io_out_T_80; // @[Lookup.scala 34:39]
  wire [32:0] _io_out_T_82 = _io_out_T_9 ? {{1'd0}, timeh} : _io_out_T_81; // @[Lookup.scala 34:39]
  wire [32:0] _io_out_T_83 = _io_out_T_7 ? {{1'd0}, cycleh} : _io_out_T_82; // @[Lookup.scala 34:39]
  wire [32:0] _io_out_T_84 = _io_out_T_5 ? {{1'd0}, instret} : _io_out_T_83; // @[Lookup.scala 34:39]
  wire [32:0] _io_out_T_85 = _io_out_T_3 ? {{1'd0}, time_} : _io_out_T_84; // @[Lookup.scala 34:39]
  wire [32:0] _io_out_T_86 = _io_out_T_1 ? {{1'd0}, cycle} : _io_out_T_85; // @[Lookup.scala 34:39]
  wire  privValid = csr_addr[9:8] <= MPP; // @[CSR.scala 257:34]
  wire  privInst = io_cmd == 3'h4; // @[CSR.scala 258:25]
  wire  _isEcall_T_2 = privInst & ~csr_addr[0]; // @[CSR.scala 259:26]
  wire  _isEcall_T_4 = ~csr_addr[8]; // @[CSR.scala 259:45]
  wire  isEcall = privInst & ~csr_addr[0] & ~csr_addr[8]; // @[CSR.scala 259:42]
  wire  isEbreak = privInst & csr_addr[0] & _isEcall_T_4; // @[CSR.scala 260:42]
  wire  isEret = _isEcall_T_2 & csr_addr[8]; // @[CSR.scala 261:41]
  wire  csrValid = _io_out_T_1 | _io_out_T_3 | _io_out_T_5 | _io_out_T_7 | _io_out_T_9 | _io_out_T_11 | _io_out_T_13 |
    _io_out_T_15 | _io_out_T_17 | _io_out_T_19 | _io_out_T_21 | _io_out_T_23 | _io_out_T_25 | _io_out_T_27 |
    _io_out_T_29 | _io_out_T_31 | _io_out_T_33 | _io_out_T_35 | _io_out_T_37 | _io_out_T_39 | _io_out_T_41 |
    _io_out_T_43 | _io_out_T_45 | _io_out_T_47 | _io_out_T_49 | _io_out_T_51 | _io_out_T_53 | _io_out_T_55 |
    _io_out_T_57; // @[CSR.scala 262:58]
  wire  csrRO = &csr_addr[11:10] | csr_addr == 12'h302; // @[CSR.scala 264:37]
  wire  wen = io_cmd == 3'h1 | io_cmd[1] & |rs1_addr; // @[CSR.scala 265:30]
  wire [31:0] _wdata_T = io_out | io_in; // @[CSR.scala 271:24]
  wire [31:0] _wdata_T_1 = ~io_in; // @[CSR.scala 272:26]
  wire [31:0] _wdata_T_2 = io_out & _wdata_T_1; // @[CSR.scala 272:24]
  wire [31:0] _wdata_T_4 = 3'h1 == io_cmd ? io_in : 32'h0; // @[Mux.scala 81:58]
  wire [31:0] _wdata_T_6 = 3'h2 == io_cmd ? _wdata_T : _wdata_T_4; // @[Mux.scala 81:58]
  wire [31:0] wdata = 3'h3 == io_cmd ? _wdata_T_2 : _wdata_T_6; // @[Mux.scala 81:58]
  wire  iaddrInvalid = io_pc_check & io_addr[1]; // @[CSR.scala 275:34]
  wire  _laddrInvalid_T_1 = |io_addr[1:0]; // @[CSR.scala 279:40]
  wire  _laddrInvalid_T_7 = 3'h2 == io_ld_type ? io_addr[0] : 3'h1 == io_ld_type & _laddrInvalid_T_1; // @[Mux.scala 81:58]
  wire  laddrInvalid = 3'h4 == io_ld_type ? io_addr[0] : _laddrInvalid_T_7; // @[Mux.scala 81:58]
  wire  saddrInvalid = 2'h2 == io_st_type ? io_addr[0] : 2'h1 == io_st_type & _laddrInvalid_T_1; // @[Mux.scala 81:58]
  reg [63:0] mtime; // @[CSR.scala 284:22]
  wire [63:0] _mtime_T = {timeh,time_}; // @[CSR.scala 285:18]
  wire [63:0] _GEN_1 = io_in_valid ? io_in_mtimecmp : {{32'd0}, mtimecmp}; // @[CSR.scala 287:21 288:14 207:21]
  wire [63:0] _GEN_279 = {{32'd0}, mtimecmp}; // @[CSR.scala 291:32]
  wire  mTimerInterrupt = mtime > _GEN_279 & MTIE & MIE; // @[CSR.scala 291:52]
  wire  _io_expt_T_6 = ~privValid; // @[CSR.scala 296:39]
  wire  _io_expt_T_8 = |io_cmd[1:0] & (~csrValid | ~privValid); // @[CSR.scala 296:22]
  wire  _io_expt_T_9 = io_illegal | iaddrInvalid | laddrInvalid | saddrInvalid | _io_expt_T_8; // @[CSR.scala 295:73]
  wire  _io_expt_T_13 = privInst & _io_expt_T_6; // @[CSR.scala 297:15]
  wire  _io_expt_T_14 = _io_expt_T_9 | wen & csrRO | _io_expt_T_13; // @[CSR.scala 296:67]
  wire [31:0] _time_T_1 = time_ + 32'h1; // @[CSR.scala 302:16]
  wire [31:0] _timeh_T_1 = timeh + 32'h1; // @[CSR.scala 303:36]
  wire [31:0] _GEN_2 = &time_ ? _timeh_T_1 : timeh; // @[CSR.scala 303:19 136:22 303:27]
  wire [31:0] _cycle_T_1 = cycle + 32'h1; // @[CSR.scala 304:18]
  wire [31:0] _cycleh_T_1 = cycleh + 32'h1; // @[CSR.scala 305:39]
  wire [31:0] _GEN_3 = &cycle ? _cycleh_T_1 : cycleh; // @[CSR.scala 305:20 138:23 305:29]
  wire  _isInstRet_T_5 = ~io_stall; // @[CSR.scala 306:88]
  wire  isInstRet = io_inst != 32'h13 & (~io_expt | isEcall | isEbreak) & ~io_stall; // @[CSR.scala 306:85]
  wire [31:0] _instret_T_1 = instret + 32'h1; // @[CSR.scala 307:40]
  wire [31:0] _GEN_4 = isInstRet ? _instret_T_1 : instret; // @[CSR.scala 307:19 139:24 307:29]
  wire [31:0] _instreth_T_1 = instreth + 32'h1; // @[CSR.scala 308:58]
  wire [31:0] _GEN_5 = isInstRet & &instret ? _instreth_T_1 : instreth; // @[CSR.scala 140:25 308:{35,46}]
  wire [31:0] _mepc_T_1 = {io_pc[31:2], 2'h0}; // @[CSR.scala 312:26]
  wire [3:0] _GEN_280 = {{2'd0}, MPP}; // @[CSR.scala 325:40]
  wire [3:0] _mcause_T_2 = 4'h8 + _GEN_280; // @[CSR.scala 325:40]
  wire [1:0] _mcause_T_3 = isEbreak ? 2'h3 : 2'h2; // @[CSR.scala 325:50]
  wire [3:0] _mcause_T_4 = isEcall ? _mcause_T_2 : {{2'd0}, _mcause_T_3}; // @[CSR.scala 325:18]
  wire [31:0] _mcause_T_5 = mTimerInterrupt ? 32'h80000007 : {{28'd0}, _mcause_T_4}; // @[CSR.scala 322:16]
  wire [31:0] _mcause_T_6 = saddrInvalid ? 32'h6 : _mcause_T_5; // @[CSR.scala 319:14]
  wire [31:0] _mepc_T_2 = {{2'd0}, wdata[31:2]}; // @[CSR.scala 370:58]
  wire [33:0] _GEN_282 = {_mepc_T_2, 2'h0}; // @[CSR.scala 370:65]
  wire [34:0] _mepc_T_3 = {{1'd0}, _GEN_282}; // @[CSR.scala 370:65]
  wire [31:0] _mcause_T_9 = wdata & 32'h8000000f; // @[CSR.scala 371:62]
  wire [34:0] _GEN_7 = csr_addr == 12'h305 ? _mepc_T_3 : {{3'd0}, mtvec}; // @[CSR.scala 188:22 381:{43,51}]
  wire [31:0] _GEN_8 = csr_addr == 12'h982 ? wdata : _GEN_5; // @[CSR.scala 380:{47,58}]
  wire [34:0] _GEN_9 = csr_addr == 12'h982 ? {{3'd0}, mtvec} : _GEN_7; // @[CSR.scala 188:22 380:47]
  wire [31:0] _GEN_10 = csr_addr == 12'h981 ? wdata : _GEN_2; // @[CSR.scala 379:{44,52}]
  wire [31:0] _GEN_11 = csr_addr == 12'h981 ? _GEN_5 : _GEN_8; // @[CSR.scala 379:44]
  wire [34:0] _GEN_12 = csr_addr == 12'h981 ? {{3'd0}, mtvec} : _GEN_9; // @[CSR.scala 188:22 379:44]
  wire [31:0] _GEN_13 = csr_addr == 12'h980 ? wdata : _GEN_3; // @[CSR.scala 378:{45,54}]
  wire [31:0] _GEN_14 = csr_addr == 12'h980 ? _GEN_2 : _GEN_10; // @[CSR.scala 378:45]
  wire [31:0] _GEN_15 = csr_addr == 12'h980 ? _GEN_5 : _GEN_11; // @[CSR.scala 378:45]
  wire [34:0] _GEN_16 = csr_addr == 12'h980 ? {{3'd0}, mtvec} : _GEN_12; // @[CSR.scala 188:22 378:45]
  wire [31:0] _GEN_17 = csr_addr == 12'h902 ? wdata : _GEN_4; // @[CSR.scala 377:{46,56}]
  wire [31:0] _GEN_18 = csr_addr == 12'h902 ? _GEN_3 : _GEN_13; // @[CSR.scala 377:46]
  wire [31:0] _GEN_19 = csr_addr == 12'h902 ? _GEN_2 : _GEN_14; // @[CSR.scala 377:46]
  wire [31:0] _GEN_20 = csr_addr == 12'h902 ? _GEN_5 : _GEN_15; // @[CSR.scala 377:46]
  wire [34:0] _GEN_21 = csr_addr == 12'h902 ? {{3'd0}, mtvec} : _GEN_16; // @[CSR.scala 188:22 377:46]
  wire [31:0] _GEN_22 = csr_addr == 12'h901 ? wdata : _time_T_1; // @[CSR.scala 376:{43,50} 302:8]
  wire [31:0] _GEN_23 = csr_addr == 12'h901 ? _GEN_4 : _GEN_17; // @[CSR.scala 376:43]
  wire [31:0] _GEN_24 = csr_addr == 12'h901 ? _GEN_3 : _GEN_18; // @[CSR.scala 376:43]
  wire [31:0] _GEN_25 = csr_addr == 12'h901 ? _GEN_2 : _GEN_19; // @[CSR.scala 376:43]
  wire [31:0] _GEN_26 = csr_addr == 12'h901 ? _GEN_5 : _GEN_20; // @[CSR.scala 376:43]
  wire [34:0] _GEN_27 = csr_addr == 12'h901 ? {{3'd0}, mtvec} : _GEN_21; // @[CSR.scala 188:22 376:43]
  wire [31:0] _GEN_28 = csr_addr == 12'h900 ? wdata : _cycle_T_1; // @[CSR.scala 375:{44,52} 304:9]
  wire [31:0] _GEN_29 = csr_addr == 12'h900 ? _time_T_1 : _GEN_22; // @[CSR.scala 375:44 302:8]
  wire [31:0] _GEN_30 = csr_addr == 12'h900 ? _GEN_4 : _GEN_23; // @[CSR.scala 375:44]
  wire [31:0] _GEN_31 = csr_addr == 12'h900 ? _GEN_3 : _GEN_24; // @[CSR.scala 375:44]
  wire [31:0] _GEN_32 = csr_addr == 12'h900 ? _GEN_2 : _GEN_25; // @[CSR.scala 375:44]
  wire [31:0] _GEN_33 = csr_addr == 12'h900 ? _GEN_5 : _GEN_26; // @[CSR.scala 375:44]
  wire [34:0] _GEN_34 = csr_addr == 12'h900 ? {{3'd0}, mtvec} : _GEN_27; // @[CSR.scala 188:22 375:44]
  wire [31:0] _GEN_35 = csr_addr == 12'h781 ? wdata : _GEN_0; // @[CSR.scala 374:{47,59}]
  wire [31:0] _GEN_36 = csr_addr == 12'h781 ? _cycle_T_1 : _GEN_28; // @[CSR.scala 374:47 304:9]
  wire [31:0] _GEN_37 = csr_addr == 12'h781 ? _time_T_1 : _GEN_29; // @[CSR.scala 374:47 302:8]
  wire [31:0] _GEN_38 = csr_addr == 12'h781 ? _GEN_4 : _GEN_30; // @[CSR.scala 374:47]
  wire [31:0] _GEN_39 = csr_addr == 12'h781 ? _GEN_3 : _GEN_31; // @[CSR.scala 374:47]
  wire [31:0] _GEN_40 = csr_addr == 12'h781 ? _GEN_2 : _GEN_32; // @[CSR.scala 374:47]
  wire [31:0] _GEN_41 = csr_addr == 12'h781 ? _GEN_5 : _GEN_33; // @[CSR.scala 374:47]
  wire [34:0] _GEN_42 = csr_addr == 12'h781 ? {{3'd0}, mtvec} : _GEN_34; // @[CSR.scala 188:22 374:47]
  wire [31:0] _GEN_43 = csr_addr == 12'h780 ? wdata : mtohost; // @[CSR.scala 215:24 373:{45,55}]
  wire [31:0] _GEN_44 = csr_addr == 12'h780 ? _GEN_0 : _GEN_35; // @[CSR.scala 373:45]
  wire [31:0] _GEN_45 = csr_addr == 12'h780 ? _cycle_T_1 : _GEN_36; // @[CSR.scala 373:45 304:9]
  wire [31:0] _GEN_46 = csr_addr == 12'h780 ? _time_T_1 : _GEN_37; // @[CSR.scala 373:45 302:8]
  wire [31:0] _GEN_47 = csr_addr == 12'h780 ? _GEN_4 : _GEN_38; // @[CSR.scala 373:45]
  wire [31:0] _GEN_48 = csr_addr == 12'h780 ? _GEN_3 : _GEN_39; // @[CSR.scala 373:45]
  wire [31:0] _GEN_49 = csr_addr == 12'h780 ? _GEN_2 : _GEN_40; // @[CSR.scala 373:45]
  wire [31:0] _GEN_50 = csr_addr == 12'h780 ? _GEN_5 : _GEN_41; // @[CSR.scala 373:45]
  wire [34:0] _GEN_51 = csr_addr == 12'h780 ? {{3'd0}, mtvec} : _GEN_42; // @[CSR.scala 188:22 373:45]
  wire [31:0] _GEN_52 = csr_addr == 12'h343 ? wdata : mbadaddr; // @[CSR.scala 213:21 372:{46,57}]
  wire [31:0] _GEN_53 = csr_addr == 12'h343 ? mtohost : _GEN_43; // @[CSR.scala 215:24 372:46]
  wire [31:0] _GEN_54 = csr_addr == 12'h343 ? _GEN_0 : _GEN_44; // @[CSR.scala 372:46]
  wire [31:0] _GEN_55 = csr_addr == 12'h343 ? _cycle_T_1 : _GEN_45; // @[CSR.scala 372:46 304:9]
  wire [31:0] _GEN_56 = csr_addr == 12'h343 ? _time_T_1 : _GEN_46; // @[CSR.scala 372:46 302:8]
  wire [31:0] _GEN_57 = csr_addr == 12'h343 ? _GEN_4 : _GEN_47; // @[CSR.scala 372:46]
  wire [31:0] _GEN_58 = csr_addr == 12'h343 ? _GEN_3 : _GEN_48; // @[CSR.scala 372:46]
  wire [31:0] _GEN_59 = csr_addr == 12'h343 ? _GEN_2 : _GEN_49; // @[CSR.scala 372:46]
  wire [31:0] _GEN_60 = csr_addr == 12'h343 ? _GEN_5 : _GEN_50; // @[CSR.scala 372:46]
  wire [34:0] _GEN_61 = csr_addr == 12'h343 ? {{3'd0}, mtvec} : _GEN_51; // @[CSR.scala 188:22 372:46]
  wire [31:0] _GEN_62 = csr_addr == 12'h342 ? _mcause_T_9 : mcause; // @[CSR.scala 212:19 371:{44,53}]
  wire [31:0] _GEN_63 = csr_addr == 12'h342 ? mbadaddr : _GEN_52; // @[CSR.scala 213:21 371:44]
  wire [31:0] _GEN_64 = csr_addr == 12'h342 ? mtohost : _GEN_53; // @[CSR.scala 215:24 371:44]
  wire [31:0] _GEN_65 = csr_addr == 12'h342 ? _GEN_0 : _GEN_54; // @[CSR.scala 371:44]
  wire [31:0] _GEN_66 = csr_addr == 12'h342 ? _cycle_T_1 : _GEN_55; // @[CSR.scala 371:44 304:9]
  wire [31:0] _GEN_67 = csr_addr == 12'h342 ? _time_T_1 : _GEN_56; // @[CSR.scala 371:44 302:8]
  wire [31:0] _GEN_68 = csr_addr == 12'h342 ? _GEN_4 : _GEN_57; // @[CSR.scala 371:44]
  wire [31:0] _GEN_69 = csr_addr == 12'h342 ? _GEN_3 : _GEN_58; // @[CSR.scala 371:44]
  wire [31:0] _GEN_70 = csr_addr == 12'h342 ? _GEN_2 : _GEN_59; // @[CSR.scala 371:44]
  wire [31:0] _GEN_71 = csr_addr == 12'h342 ? _GEN_5 : _GEN_60; // @[CSR.scala 371:44]
  wire [34:0] _GEN_72 = csr_addr == 12'h342 ? {{3'd0}, mtvec} : _GEN_61; // @[CSR.scala 188:22 371:44]
  wire [34:0] _GEN_73 = csr_addr == 12'h341 ? _mepc_T_3 : {{3'd0}, mepc}; // @[CSR.scala 211:17 370:{42,49}]
  wire [31:0] _GEN_74 = csr_addr == 12'h341 ? mcause : _GEN_62; // @[CSR.scala 212:19 370:42]
  wire [31:0] _GEN_75 = csr_addr == 12'h341 ? mbadaddr : _GEN_63; // @[CSR.scala 213:21 370:42]
  wire [31:0] _GEN_76 = csr_addr == 12'h341 ? mtohost : _GEN_64; // @[CSR.scala 215:24 370:42]
  wire [31:0] _GEN_77 = csr_addr == 12'h341 ? _GEN_0 : _GEN_65; // @[CSR.scala 370:42]
  wire [31:0] _GEN_78 = csr_addr == 12'h341 ? _cycle_T_1 : _GEN_66; // @[CSR.scala 370:42 304:9]
  wire [31:0] _GEN_79 = csr_addr == 12'h341 ? _time_T_1 : _GEN_67; // @[CSR.scala 370:42 302:8]
  wire [31:0] _GEN_80 = csr_addr == 12'h341 ? _GEN_4 : _GEN_68; // @[CSR.scala 370:42]
  wire [31:0] _GEN_81 = csr_addr == 12'h341 ? _GEN_3 : _GEN_69; // @[CSR.scala 370:42]
  wire [31:0] _GEN_82 = csr_addr == 12'h341 ? _GEN_2 : _GEN_70; // @[CSR.scala 370:42]
  wire [31:0] _GEN_83 = csr_addr == 12'h341 ? _GEN_5 : _GEN_71; // @[CSR.scala 370:42]
  wire [34:0] _GEN_84 = csr_addr == 12'h341 ? {{3'd0}, mtvec} : _GEN_72; // @[CSR.scala 188:22 370:42]
  wire [31:0] _GEN_85 = csr_addr == 12'h340 ? wdata : mscratch; // @[CSR.scala 209:21 369:{46,57}]
  wire [34:0] _GEN_86 = csr_addr == 12'h340 ? {{3'd0}, mepc} : _GEN_73; // @[CSR.scala 211:17 369:46]
  wire [31:0] _GEN_87 = csr_addr == 12'h340 ? mcause : _GEN_74; // @[CSR.scala 212:19 369:46]
  wire [31:0] _GEN_88 = csr_addr == 12'h340 ? mbadaddr : _GEN_75; // @[CSR.scala 213:21 369:46]
  wire [31:0] _GEN_89 = csr_addr == 12'h340 ? mtohost : _GEN_76; // @[CSR.scala 215:24 369:46]
  wire [31:0] _GEN_90 = csr_addr == 12'h340 ? _GEN_0 : _GEN_77; // @[CSR.scala 369:46]
  wire [31:0] _GEN_91 = csr_addr == 12'h340 ? _cycle_T_1 : _GEN_78; // @[CSR.scala 369:46 304:9]
  wire [31:0] _GEN_92 = csr_addr == 12'h340 ? _time_T_1 : _GEN_79; // @[CSR.scala 369:46 302:8]
  wire [31:0] _GEN_93 = csr_addr == 12'h340 ? _GEN_4 : _GEN_80; // @[CSR.scala 369:46]
  wire [31:0] _GEN_94 = csr_addr == 12'h340 ? _GEN_3 : _GEN_81; // @[CSR.scala 369:46]
  wire [31:0] _GEN_95 = csr_addr == 12'h340 ? _GEN_2 : _GEN_82; // @[CSR.scala 369:46]
  wire [31:0] _GEN_96 = csr_addr == 12'h340 ? _GEN_5 : _GEN_83; // @[CSR.scala 369:46]
  wire [34:0] _GEN_97 = csr_addr == 12'h340 ? {{3'd0}, mtvec} : _GEN_84; // @[CSR.scala 188:22 369:46]
  wire [63:0] _GEN_98 = csr_addr == 12'h321 ? {{32'd0}, wdata} : _GEN_1; // @[CSR.scala 368:{46,57}]
  wire [31:0] _GEN_99 = csr_addr == 12'h321 ? mscratch : _GEN_85; // @[CSR.scala 209:21 368:46]
  wire [34:0] _GEN_100 = csr_addr == 12'h321 ? {{3'd0}, mepc} : _GEN_86; // @[CSR.scala 211:17 368:46]
  wire [31:0] _GEN_101 = csr_addr == 12'h321 ? mcause : _GEN_87; // @[CSR.scala 212:19 368:46]
  wire [31:0] _GEN_102 = csr_addr == 12'h321 ? mbadaddr : _GEN_88; // @[CSR.scala 213:21 368:46]
  wire [31:0] _GEN_103 = csr_addr == 12'h321 ? mtohost : _GEN_89; // @[CSR.scala 215:24 368:46]
  wire [31:0] _GEN_104 = csr_addr == 12'h321 ? _GEN_0 : _GEN_90; // @[CSR.scala 368:46]
  wire [31:0] _GEN_105 = csr_addr == 12'h321 ? _cycle_T_1 : _GEN_91; // @[CSR.scala 368:46 304:9]
  wire [31:0] _GEN_106 = csr_addr == 12'h321 ? _time_T_1 : _GEN_92; // @[CSR.scala 368:46 302:8]
  wire [31:0] _GEN_107 = csr_addr == 12'h321 ? _GEN_4 : _GEN_93; // @[CSR.scala 368:46]
  wire [31:0] _GEN_108 = csr_addr == 12'h321 ? _GEN_3 : _GEN_94; // @[CSR.scala 368:46]
  wire [31:0] _GEN_109 = csr_addr == 12'h321 ? _GEN_2 : _GEN_95; // @[CSR.scala 368:46]
  wire [31:0] _GEN_110 = csr_addr == 12'h321 ? _GEN_5 : _GEN_96; // @[CSR.scala 368:46]
  wire [34:0] _GEN_111 = csr_addr == 12'h321 ? {{3'd0}, mtvec} : _GEN_97; // @[CSR.scala 188:22 368:46]
  wire [31:0] _GEN_112 = csr_addr == 12'h741 ? wdata : _GEN_109; // @[CSR.scala 367:{44,52}]
  wire [63:0] _GEN_113 = csr_addr == 12'h741 ? _GEN_1 : _GEN_98; // @[CSR.scala 367:44]
  wire [31:0] _GEN_114 = csr_addr == 12'h741 ? mscratch : _GEN_99; // @[CSR.scala 209:21 367:44]
  wire [34:0] _GEN_115 = csr_addr == 12'h741 ? {{3'd0}, mepc} : _GEN_100; // @[CSR.scala 211:17 367:44]
  wire [31:0] _GEN_116 = csr_addr == 12'h741 ? mcause : _GEN_101; // @[CSR.scala 212:19 367:44]
  wire [31:0] _GEN_117 = csr_addr == 12'h741 ? mbadaddr : _GEN_102; // @[CSR.scala 213:21 367:44]
  wire [31:0] _GEN_118 = csr_addr == 12'h741 ? mtohost : _GEN_103; // @[CSR.scala 215:24 367:44]
  wire [31:0] _GEN_119 = csr_addr == 12'h741 ? _GEN_0 : _GEN_104; // @[CSR.scala 367:44]
  wire [31:0] _GEN_120 = csr_addr == 12'h741 ? _cycle_T_1 : _GEN_105; // @[CSR.scala 367:44 304:9]
  wire [31:0] _GEN_121 = csr_addr == 12'h741 ? _time_T_1 : _GEN_106; // @[CSR.scala 367:44 302:8]
  wire [31:0] _GEN_122 = csr_addr == 12'h741 ? _GEN_4 : _GEN_107; // @[CSR.scala 367:44]
  wire [31:0] _GEN_123 = csr_addr == 12'h741 ? _GEN_3 : _GEN_108; // @[CSR.scala 367:44]
  wire [31:0] _GEN_124 = csr_addr == 12'h741 ? _GEN_5 : _GEN_110; // @[CSR.scala 367:44]
  wire [34:0] _GEN_125 = csr_addr == 12'h741 ? {{3'd0}, mtvec} : _GEN_111; // @[CSR.scala 188:22 367:44]
  wire [31:0] _GEN_126 = csr_addr == 12'h701 ? wdata : _GEN_121; // @[CSR.scala 366:{43,50}]
  wire [31:0] _GEN_127 = csr_addr == 12'h701 ? _GEN_2 : _GEN_112; // @[CSR.scala 366:43]
  wire [63:0] _GEN_128 = csr_addr == 12'h701 ? _GEN_1 : _GEN_113; // @[CSR.scala 366:43]
  wire [31:0] _GEN_129 = csr_addr == 12'h701 ? mscratch : _GEN_114; // @[CSR.scala 209:21 366:43]
  wire [34:0] _GEN_130 = csr_addr == 12'h701 ? {{3'd0}, mepc} : _GEN_115; // @[CSR.scala 211:17 366:43]
  wire [31:0] _GEN_131 = csr_addr == 12'h701 ? mcause : _GEN_116; // @[CSR.scala 212:19 366:43]
  wire [31:0] _GEN_132 = csr_addr == 12'h701 ? mbadaddr : _GEN_117; // @[CSR.scala 213:21 366:43]
  wire [31:0] _GEN_133 = csr_addr == 12'h701 ? mtohost : _GEN_118; // @[CSR.scala 215:24 366:43]
  wire [31:0] _GEN_134 = csr_addr == 12'h701 ? _GEN_0 : _GEN_119; // @[CSR.scala 366:43]
  wire [31:0] _GEN_135 = csr_addr == 12'h701 ? _cycle_T_1 : _GEN_120; // @[CSR.scala 366:43 304:9]
  wire [31:0] _GEN_136 = csr_addr == 12'h701 ? _GEN_4 : _GEN_122; // @[CSR.scala 366:43]
  wire [31:0] _GEN_137 = csr_addr == 12'h701 ? _GEN_3 : _GEN_123; // @[CSR.scala 366:43]
  wire [31:0] _GEN_138 = csr_addr == 12'h701 ? _GEN_5 : _GEN_124; // @[CSR.scala 366:43]
  wire [34:0] _GEN_139 = csr_addr == 12'h701 ? {{3'd0}, mtvec} : _GEN_125; // @[CSR.scala 188:22 366:43]
  wire  _GEN_140 = csr_addr == 12'h304 ? wdata[7] : MTIE; // @[CSR.scala 362:41 363:16 195:21]
  wire  _GEN_141 = csr_addr == 12'h304 ? wdata[3] : MSIE; // @[CSR.scala 362:41 364:16 201:21]
  wire [31:0] _GEN_142 = csr_addr == 12'h304 ? _time_T_1 : _GEN_126; // @[CSR.scala 362:41 302:8]
  wire [31:0] _GEN_143 = csr_addr == 12'h304 ? _GEN_2 : _GEN_127; // @[CSR.scala 362:41]
  wire [63:0] _GEN_144 = csr_addr == 12'h304 ? _GEN_1 : _GEN_128; // @[CSR.scala 362:41]
  wire [31:0] _GEN_145 = csr_addr == 12'h304 ? mscratch : _GEN_129; // @[CSR.scala 209:21 362:41]
  wire [34:0] _GEN_146 = csr_addr == 12'h304 ? {{3'd0}, mepc} : _GEN_130; // @[CSR.scala 211:17 362:41]
  wire [31:0] _GEN_147 = csr_addr == 12'h304 ? mcause : _GEN_131; // @[CSR.scala 212:19 362:41]
  wire [31:0] _GEN_148 = csr_addr == 12'h304 ? mbadaddr : _GEN_132; // @[CSR.scala 213:21 362:41]
  wire [31:0] _GEN_149 = csr_addr == 12'h304 ? mtohost : _GEN_133; // @[CSR.scala 215:24 362:41]
  wire [31:0] _GEN_150 = csr_addr == 12'h304 ? _GEN_0 : _GEN_134; // @[CSR.scala 362:41]
  wire [31:0] _GEN_151 = csr_addr == 12'h304 ? _cycle_T_1 : _GEN_135; // @[CSR.scala 362:41 304:9]
  wire [31:0] _GEN_152 = csr_addr == 12'h304 ? _GEN_4 : _GEN_136; // @[CSR.scala 362:41]
  wire [31:0] _GEN_153 = csr_addr == 12'h304 ? _GEN_3 : _GEN_137; // @[CSR.scala 362:41]
  wire [31:0] _GEN_154 = csr_addr == 12'h304 ? _GEN_5 : _GEN_138; // @[CSR.scala 362:41]
  wire [34:0] _GEN_155 = csr_addr == 12'h304 ? {{3'd0}, mtvec} : _GEN_139; // @[CSR.scala 188:22 362:41]
  wire  _GEN_156 = csr_addr == 12'h344 ? wdata[7] : MTIP; // @[CSR.scala 358:41 359:16 192:21]
  wire  _GEN_157 = csr_addr == 12'h344 ? wdata[3] : MSIP; // @[CSR.scala 358:41 360:16 198:21]
  wire  _GEN_158 = csr_addr == 12'h344 ? MTIE : _GEN_140; // @[CSR.scala 195:21 358:41]
  wire  _GEN_159 = csr_addr == 12'h344 ? MSIE : _GEN_141; // @[CSR.scala 201:21 358:41]
  wire [31:0] _GEN_160 = csr_addr == 12'h344 ? _time_T_1 : _GEN_142; // @[CSR.scala 358:41 302:8]
  wire [31:0] _GEN_161 = csr_addr == 12'h344 ? _GEN_2 : _GEN_143; // @[CSR.scala 358:41]
  wire [63:0] _GEN_162 = csr_addr == 12'h344 ? _GEN_1 : _GEN_144; // @[CSR.scala 358:41]
  wire [31:0] _GEN_163 = csr_addr == 12'h344 ? mscratch : _GEN_145; // @[CSR.scala 209:21 358:41]
  wire [34:0] _GEN_164 = csr_addr == 12'h344 ? {{3'd0}, mepc} : _GEN_146; // @[CSR.scala 211:17 358:41]
  wire [31:0] _GEN_165 = csr_addr == 12'h344 ? mcause : _GEN_147; // @[CSR.scala 212:19 358:41]
  wire [31:0] _GEN_166 = csr_addr == 12'h344 ? mbadaddr : _GEN_148; // @[CSR.scala 213:21 358:41]
  wire [31:0] _GEN_167 = csr_addr == 12'h344 ? mtohost : _GEN_149; // @[CSR.scala 215:24 358:41]
  wire [31:0] _GEN_168 = csr_addr == 12'h344 ? _GEN_0 : _GEN_150; // @[CSR.scala 358:41]
  wire [31:0] _GEN_169 = csr_addr == 12'h344 ? _cycle_T_1 : _GEN_151; // @[CSR.scala 358:41 304:9]
  wire [31:0] _GEN_170 = csr_addr == 12'h344 ? _GEN_4 : _GEN_152; // @[CSR.scala 358:41]
  wire [31:0] _GEN_171 = csr_addr == 12'h344 ? _GEN_3 : _GEN_153; // @[CSR.scala 358:41]
  wire [31:0] _GEN_172 = csr_addr == 12'h344 ? _GEN_5 : _GEN_154; // @[CSR.scala 358:41]
  wire [34:0] _GEN_173 = csr_addr == 12'h344 ? {{3'd0}, mtvec} : _GEN_155; // @[CSR.scala 188:22 358:41]
  wire  _GEN_174 = csr_addr == 12'h300 ? wdata[3] : MIE; // @[CSR.scala 349:38 354:13 170:20]
  wire  _GEN_175 = csr_addr == 12'h300 ? wdata[7] : MPIE; // @[CSR.scala 349:38 355:14 171:21]
  wire [1:0] _GEN_176 = csr_addr == 12'h300 ? wdata[12:11] : MPP; // @[CSR.scala 349:38 356:13 172:20]
  wire  _GEN_177 = csr_addr == 12'h300 ? MTIP : _GEN_156; // @[CSR.scala 192:21 349:38]
  wire  _GEN_178 = csr_addr == 12'h300 ? MSIP : _GEN_157; // @[CSR.scala 198:21 349:38]
  wire  _GEN_179 = csr_addr == 12'h300 ? MTIE : _GEN_158; // @[CSR.scala 195:21 349:38]
  wire  _GEN_180 = csr_addr == 12'h300 ? MSIE : _GEN_159; // @[CSR.scala 201:21 349:38]
  wire [31:0] _GEN_181 = csr_addr == 12'h300 ? _time_T_1 : _GEN_160; // @[CSR.scala 349:38 302:8]
  wire [31:0] _GEN_182 = csr_addr == 12'h300 ? _GEN_2 : _GEN_161; // @[CSR.scala 349:38]
  wire [63:0] _GEN_183 = csr_addr == 12'h300 ? _GEN_1 : _GEN_162; // @[CSR.scala 349:38]
  wire [31:0] _GEN_184 = csr_addr == 12'h300 ? mscratch : _GEN_163; // @[CSR.scala 209:21 349:38]
  wire [34:0] _GEN_185 = csr_addr == 12'h300 ? {{3'd0}, mepc} : _GEN_164; // @[CSR.scala 211:17 349:38]
  wire [31:0] _GEN_186 = csr_addr == 12'h300 ? mcause : _GEN_165; // @[CSR.scala 212:19 349:38]
  wire [31:0] _GEN_187 = csr_addr == 12'h300 ? mbadaddr : _GEN_166; // @[CSR.scala 213:21 349:38]
  wire [31:0] _GEN_188 = csr_addr == 12'h300 ? mtohost : _GEN_167; // @[CSR.scala 215:24 349:38]
  wire [31:0] _GEN_189 = csr_addr == 12'h300 ? _GEN_0 : _GEN_168; // @[CSR.scala 349:38]
  wire [31:0] _GEN_190 = csr_addr == 12'h300 ? _cycle_T_1 : _GEN_169; // @[CSR.scala 349:38 304:9]
  wire [31:0] _GEN_191 = csr_addr == 12'h300 ? _GEN_4 : _GEN_170; // @[CSR.scala 349:38]
  wire [31:0] _GEN_192 = csr_addr == 12'h300 ? _GEN_3 : _GEN_171; // @[CSR.scala 349:38]
  wire [31:0] _GEN_193 = csr_addr == 12'h300 ? _GEN_5 : _GEN_172; // @[CSR.scala 349:38]
  wire [34:0] _GEN_194 = csr_addr == 12'h300 ? {{3'd0}, mtvec} : _GEN_173; // @[CSR.scala 188:22 349:38]
  wire  _GEN_195 = wen ? _GEN_174 : MIE; // @[CSR.scala 170:20 348:21]
  wire  _GEN_196 = wen ? _GEN_175 : MPIE; // @[CSR.scala 171:21 348:21]
  wire [1:0] _GEN_197 = wen ? _GEN_176 : MPP; // @[CSR.scala 172:20 348:21]
  wire  _GEN_198 = wen ? _GEN_177 : MTIP; // @[CSR.scala 192:21 348:21]
  wire  _GEN_199 = wen ? _GEN_178 : MSIP; // @[CSR.scala 198:21 348:21]
  wire  _GEN_200 = wen ? _GEN_179 : MTIE; // @[CSR.scala 195:21 348:21]
  wire  _GEN_201 = wen ? _GEN_180 : MSIE; // @[CSR.scala 201:21 348:21]
  wire [31:0] _GEN_202 = wen ? _GEN_181 : _time_T_1; // @[CSR.scala 348:21 302:8]
  wire [31:0] _GEN_203 = wen ? _GEN_182 : _GEN_2; // @[CSR.scala 348:21]
  wire [63:0] _GEN_204 = wen ? _GEN_183 : _GEN_1; // @[CSR.scala 348:21]
  wire [34:0] _GEN_206 = wen ? _GEN_185 : {{3'd0}, mepc}; // @[CSR.scala 211:17 348:21]
  wire [31:0] _GEN_209 = wen ? _GEN_188 : mtohost; // @[CSR.scala 348:21 215:24]
  wire [31:0] _GEN_211 = wen ? _GEN_190 : _cycle_T_1; // @[CSR.scala 348:21 304:9]
  wire [31:0] _GEN_212 = wen ? _GEN_191 : _GEN_4; // @[CSR.scala 348:21]
  wire [31:0] _GEN_213 = wen ? _GEN_192 : _GEN_3; // @[CSR.scala 348:21]
  wire [31:0] _GEN_214 = wen ? _GEN_193 : _GEN_5; // @[CSR.scala 348:21]
  wire [34:0] _GEN_215 = wen ? _GEN_194 : {{3'd0}, mtvec}; // @[CSR.scala 348:21 188:22]
  wire  _GEN_217 = isEret | _GEN_196; // @[CSR.scala 339:24 341:12]
  wire [63:0] _GEN_225 = isEret ? _GEN_1 : _GEN_204; // @[CSR.scala 339:24]
  wire [34:0] _GEN_227 = isEret ? {{3'd0}, mepc} : _GEN_206; // @[CSR.scala 211:17 339:24]
  wire [34:0] _GEN_236 = isEret ? {{3'd0}, mtvec} : _GEN_215; // @[CSR.scala 188:22 339:24]
  wire [34:0] _GEN_237 = io_expt ? {{3'd0}, _mepc_T_1} : _GEN_227; // @[CSR.scala 311:19 312:12]
  wire [63:0] _GEN_249 = io_expt ? _GEN_1 : _GEN_225; // @[CSR.scala 311:19]
  wire [34:0] _GEN_257 = io_expt ? {{3'd0}, mtvec} : _GEN_236; // @[CSR.scala 311:19 188:22]
  wire [34:0] _GEN_258 = _isInstRet_T_5 ? _GEN_237 : {{3'd0}, mepc}; // @[CSR.scala 211:17 310:19]
  wire [63:0] _GEN_270 = _isInstRet_T_5 ? _GEN_249 : _GEN_1; // @[CSR.scala 310:19]
  wire [34:0] _GEN_278 = _isInstRet_T_5 ? _GEN_257 : {{3'd0}, mtvec}; // @[CSR.scala 310:19 188:22]
  wire [34:0] _GEN_285 = reset ? 35'h30000000 : _GEN_278; // @[CSR.scala 188:{22,22}]
  assign io_out = _io_out_T_86[31:0]; // @[CSR.scala 254:10]
  assign io_expt = _io_expt_T_14 | isEcall | isEbreak | mTimerInterrupt; // @[CSR.scala 297:53]
  assign io_evec = mtvec; // @[CSR.scala 298:11]
  assign io_epc = mepc; // @[CSR.scala 299:10]
  assign io_host_tohost = mtohost; // @[CSR.scala 217:18]
  always @(posedge clock) begin
    if (reset) begin // @[CSR.scala 135:21]
      time_ <= 32'h0; // @[CSR.scala 135:21]
    end else if (_isInstRet_T_5) begin // @[CSR.scala 310:19]
      if (io_expt) begin // @[CSR.scala 311:19]
        time_ <= _time_T_1; // @[CSR.scala 302:8]
      end else if (isEret) begin // @[CSR.scala 339:24]
        time_ <= _time_T_1; // @[CSR.scala 302:8]
      end else begin
        time_ <= _GEN_202;
      end
    end else begin
      time_ <= _time_T_1; // @[CSR.scala 302:8]
    end
    if (reset) begin // @[CSR.scala 136:22]
      timeh <= 32'h0; // @[CSR.scala 136:22]
    end else if (_isInstRet_T_5) begin // @[CSR.scala 310:19]
      if (io_expt) begin // @[CSR.scala 311:19]
        timeh <= _GEN_2;
      end else if (isEret) begin // @[CSR.scala 339:24]
        timeh <= _GEN_2;
      end else begin
        timeh <= _GEN_203;
      end
    end else begin
      timeh <= _GEN_2;
    end
    if (reset) begin // @[CSR.scala 137:22]
      cycle <= 32'h0; // @[CSR.scala 137:22]
    end else if (_isInstRet_T_5) begin // @[CSR.scala 310:19]
      if (io_expt) begin // @[CSR.scala 311:19]
        cycle <= _cycle_T_1; // @[CSR.scala 304:9]
      end else if (isEret) begin // @[CSR.scala 339:24]
        cycle <= _cycle_T_1; // @[CSR.scala 304:9]
      end else begin
        cycle <= _GEN_211;
      end
    end else begin
      cycle <= _cycle_T_1; // @[CSR.scala 304:9]
    end
    if (reset) begin // @[CSR.scala 138:23]
      cycleh <= 32'h0; // @[CSR.scala 138:23]
    end else if (_isInstRet_T_5) begin // @[CSR.scala 310:19]
      if (io_expt) begin // @[CSR.scala 311:19]
        cycleh <= _GEN_3;
      end else if (isEret) begin // @[CSR.scala 339:24]
        cycleh <= _GEN_3;
      end else begin
        cycleh <= _GEN_213;
      end
    end else begin
      cycleh <= _GEN_3;
    end
    if (reset) begin // @[CSR.scala 139:24]
      instret <= 32'h0; // @[CSR.scala 139:24]
    end else if (_isInstRet_T_5) begin // @[CSR.scala 310:19]
      if (io_expt) begin // @[CSR.scala 311:19]
        instret <= _GEN_4;
      end else if (isEret) begin // @[CSR.scala 339:24]
        instret <= _GEN_4;
      end else begin
        instret <= _GEN_212;
      end
    end else begin
      instret <= _GEN_4;
    end
    if (reset) begin // @[CSR.scala 140:25]
      instreth <= 32'h0; // @[CSR.scala 140:25]
    end else if (_isInstRet_T_5) begin // @[CSR.scala 310:19]
      if (io_expt) begin // @[CSR.scala 311:19]
        instreth <= _GEN_5;
      end else if (isEret) begin // @[CSR.scala 339:24]
        instreth <= _GEN_5;
      end else begin
        instreth <= _GEN_214;
      end
    end else begin
      instreth <= _GEN_5;
    end
    if (reset) begin // @[CSR.scala 170:20]
      MIE <= 1'h0; // @[CSR.scala 170:20]
    end else if (_isInstRet_T_5) begin // @[CSR.scala 310:19]
      if (io_expt) begin // @[CSR.scala 311:19]
        MIE <= 1'h0; // @[CSR.scala 331:11]
      end else if (isEret) begin // @[CSR.scala 339:24]
        MIE <= MPIE; // @[CSR.scala 340:11]
      end else begin
        MIE <= _GEN_195;
      end
    end
    if (reset) begin // @[CSR.scala 171:21]
      MPIE <= 1'h0; // @[CSR.scala 171:21]
    end else if (_isInstRet_T_5) begin // @[CSR.scala 310:19]
      if (io_expt) begin // @[CSR.scala 311:19]
        MPIE <= MIE; // @[CSR.scala 330:12]
      end else begin
        MPIE <= _GEN_217;
      end
    end
    if (reset) begin // @[CSR.scala 172:20]
      MPP <= 2'h3; // @[CSR.scala 172:20]
    end else if (_isInstRet_T_5) begin // @[CSR.scala 310:19]
      if (io_expt) begin // @[CSR.scala 311:19]
        MPP <= 2'h3; // @[CSR.scala 332:11]
      end else if (isEret) begin // @[CSR.scala 339:24]
        MPP <= 2'h0; // @[CSR.scala 342:11]
      end else begin
        MPP <= _GEN_197;
      end
    end
    mtvec <= _GEN_285[31:0]; // @[CSR.scala 188:{22,22}]
    if (reset) begin // @[CSR.scala 192:21]
      MTIP <= 1'h0; // @[CSR.scala 192:21]
    end else if (_isInstRet_T_5) begin // @[CSR.scala 310:19]
      if (!(io_expt)) begin // @[CSR.scala 311:19]
        if (!(isEret)) begin // @[CSR.scala 339:24]
          MTIP <= _GEN_198;
        end
      end
    end
    if (reset) begin // @[CSR.scala 195:21]
      MTIE <= 1'h0; // @[CSR.scala 195:21]
    end else if (_isInstRet_T_5) begin // @[CSR.scala 310:19]
      if (!(io_expt)) begin // @[CSR.scala 311:19]
        if (!(isEret)) begin // @[CSR.scala 339:24]
          MTIE <= _GEN_200;
        end
      end
    end
    if (reset) begin // @[CSR.scala 198:21]
      MSIP <= 1'h0; // @[CSR.scala 198:21]
    end else if (_isInstRet_T_5) begin // @[CSR.scala 310:19]
      if (!(io_expt)) begin // @[CSR.scala 311:19]
        if (!(isEret)) begin // @[CSR.scala 339:24]
          MSIP <= _GEN_199;
        end
      end
    end
    if (reset) begin // @[CSR.scala 201:21]
      MSIE <= 1'h0; // @[CSR.scala 201:21]
    end else if (_isInstRet_T_5) begin // @[CSR.scala 310:19]
      if (!(io_expt)) begin // @[CSR.scala 311:19]
        if (!(isEret)) begin // @[CSR.scala 339:24]
          MSIE <= _GEN_201;
        end
      end
    end
    mtimecmp <= _GEN_270[31:0];
    if (_isInstRet_T_5) begin // @[CSR.scala 310:19]
      if (!(io_expt)) begin // @[CSR.scala 311:19]
        if (!(isEret)) begin // @[CSR.scala 339:24]
          if (wen) begin // @[CSR.scala 348:21]
            mscratch <= _GEN_184;
          end
        end
      end
    end
    mepc <= _GEN_258[31:0];
    if (_isInstRet_T_5) begin // @[CSR.scala 310:19]
      if (io_expt) begin // @[CSR.scala 311:19]
        if (iaddrInvalid) begin // @[CSR.scala 313:20]
          mcause <= 32'h0;
        end else if (laddrInvalid) begin // @[CSR.scala 316:12]
          mcause <= 32'h4;
        end else begin
          mcause <= _mcause_T_6;
        end
      end else if (!(isEret)) begin // @[CSR.scala 339:24]
        if (wen) begin // @[CSR.scala 348:21]
          mcause <= _GEN_186;
        end
      end
    end
    if (_isInstRet_T_5) begin // @[CSR.scala 310:19]
      if (io_expt) begin // @[CSR.scala 311:19]
        if (iaddrInvalid | laddrInvalid | saddrInvalid) begin // @[CSR.scala 338:58]
          mbadaddr <= io_addr; // @[CSR.scala 338:69]
        end
      end else if (!(isEret)) begin // @[CSR.scala 339:24]
        if (wen) begin // @[CSR.scala 348:21]
          mbadaddr <= _GEN_187;
        end
      end
    end
    if (reset) begin // @[CSR.scala 215:24]
      mtohost <= 32'h0; // @[CSR.scala 215:24]
    end else if (_isInstRet_T_5) begin // @[CSR.scala 310:19]
      if (!(io_expt)) begin // @[CSR.scala 311:19]
        if (!(isEret)) begin // @[CSR.scala 339:24]
          mtohost <= _GEN_209;
        end
      end
    end
    if (_isInstRet_T_5) begin // @[CSR.scala 310:19]
      if (io_expt) begin // @[CSR.scala 311:19]
        mfromhost <= _GEN_0;
      end else if (isEret) begin // @[CSR.scala 339:24]
        mfromhost <= _GEN_0;
      end else if (wen) begin // @[CSR.scala 348:21]
        mfromhost <= _GEN_189;
      end else begin
        mfromhost <= _GEN_0;
      end
    end else begin
      mfromhost <= _GEN_0;
    end
    if (reset) begin // @[CSR.scala 284:22]
      mtime <= 64'h0; // @[CSR.scala 284:22]
    end else begin
      mtime <= _mtime_T; // @[CSR.scala 285:9]
    end
  end
// Register and memory initialization
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
`ifdef FIRRTL_BEFORE_INITIAL
`FIRRTL_BEFORE_INITIAL
`endif
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
`ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  time_ = _RAND_0[31:0];
  _RAND_1 = {1{`RANDOM}};
  timeh = _RAND_1[31:0];
  _RAND_2 = {1{`RANDOM}};
  cycle = _RAND_2[31:0];
  _RAND_3 = {1{`RANDOM}};
  cycleh = _RAND_3[31:0];
  _RAND_4 = {1{`RANDOM}};
  instret = _RAND_4[31:0];
  _RAND_5 = {1{`RANDOM}};
  instreth = _RAND_5[31:0];
  _RAND_6 = {1{`RANDOM}};
  MIE = _RAND_6[0:0];
  _RAND_7 = {1{`RANDOM}};
  MPIE = _RAND_7[0:0];
  _RAND_8 = {1{`RANDOM}};
  MPP = _RAND_8[1:0];
  _RAND_9 = {1{`RANDOM}};
  mtvec = _RAND_9[31:0];
  _RAND_10 = {1{`RANDOM}};
  MTIP = _RAND_10[0:0];
  _RAND_11 = {1{`RANDOM}};
  MTIE = _RAND_11[0:0];
  _RAND_12 = {1{`RANDOM}};
  MSIP = _RAND_12[0:0];
  _RAND_13 = {1{`RANDOM}};
  MSIE = _RAND_13[0:0];
  _RAND_14 = {1{`RANDOM}};
  mtimecmp = _RAND_14[31:0];
  _RAND_15 = {1{`RANDOM}};
  mscratch = _RAND_15[31:0];
  _RAND_16 = {1{`RANDOM}};
  mepc = _RAND_16[31:0];
  _RAND_17 = {1{`RANDOM}};
  mcause = _RAND_17[31:0];
  _RAND_18 = {1{`RANDOM}};
  mbadaddr = _RAND_18[31:0];
  _RAND_19 = {1{`RANDOM}};
  mtohost = _RAND_19[31:0];
  _RAND_20 = {1{`RANDOM}};
  mfromhost = _RAND_20[31:0];
  _RAND_21 = {2{`RANDOM}};
  mtime = _RAND_21[63:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
module RegFile(
  input         clock,
  input  [4:0]  io_raddr1,
  input  [4:0]  io_raddr2,
  output [31:0] io_rdata1,
  output [31:0] io_rdata2,
  input         io_wen,
  input  [4:0]  io_waddr,
  input  [31:0] io_wdata
);
`ifdef RANDOMIZE_MEM_INIT
  reg [31:0] _RAND_0;
`endif // RANDOMIZE_MEM_INIT
  reg [31:0] regs [0:31]; // @[RegFile.scala 19:17]
  wire  regs_io_rdata1_MPORT_en; // @[RegFile.scala 19:17]
  wire [4:0] regs_io_rdata1_MPORT_addr; // @[RegFile.scala 19:17]
  wire [31:0] regs_io_rdata1_MPORT_data; // @[RegFile.scala 19:17]
  wire  regs_io_rdata2_MPORT_en; // @[RegFile.scala 19:17]
  wire [4:0] regs_io_rdata2_MPORT_addr; // @[RegFile.scala 19:17]
  wire [31:0] regs_io_rdata2_MPORT_data; // @[RegFile.scala 19:17]
  wire [31:0] regs_MPORT_data; // @[RegFile.scala 19:17]
  wire [4:0] regs_MPORT_addr; // @[RegFile.scala 19:17]
  wire  regs_MPORT_mask; // @[RegFile.scala 19:17]
  wire  regs_MPORT_en; // @[RegFile.scala 19:17]
  wire  _T = |io_waddr; // @[RegFile.scala 22:26]
  assign regs_io_rdata1_MPORT_en = 1'h1;
  assign regs_io_rdata1_MPORT_addr = io_raddr1;
  assign regs_io_rdata1_MPORT_data = regs[regs_io_rdata1_MPORT_addr]; // @[RegFile.scala 19:17]
  assign regs_io_rdata2_MPORT_en = 1'h1;
  assign regs_io_rdata2_MPORT_addr = io_raddr2;
  assign regs_io_rdata2_MPORT_data = regs[regs_io_rdata2_MPORT_addr]; // @[RegFile.scala 19:17]
  assign regs_MPORT_data = io_wdata;
  assign regs_MPORT_addr = io_waddr;
  assign regs_MPORT_mask = 1'h1;
  assign regs_MPORT_en = io_wen & _T;
  assign io_rdata1 = |io_raddr1 ? regs_io_rdata1_MPORT_data : 32'h0; // @[RegFile.scala 20:19]
  assign io_rdata2 = |io_raddr2 ? regs_io_rdata2_MPORT_data : 32'h0; // @[RegFile.scala 21:19]
  always @(posedge clock) begin
    if (regs_MPORT_en & regs_MPORT_mask) begin
      regs[regs_MPORT_addr] <= regs_MPORT_data; // @[RegFile.scala 19:17]
    end
  end
// Register and memory initialization
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
`ifdef FIRRTL_BEFORE_INITIAL
`FIRRTL_BEFORE_INITIAL
`endif
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
`ifdef RANDOMIZE_MEM_INIT
  _RAND_0 = {1{`RANDOM}};
  for (initvar = 0; initvar < 32; initvar = initvar+1)
    regs[initvar] = _RAND_0[31:0];
`endif // RANDOMIZE_MEM_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
module AluArea(
  input  [31:0] io_A,
  input  [31:0] io_B,
  input  [3:0]  io_alu_op,
  output [31:0] io_out,
  output [31:0] io_sum
);
  wire [31:0] _sum_T_2 = 32'h0 - io_B; // @[Alu.scala 67:38]
  wire [31:0] _sum_T_3 = io_alu_op[0] ? _sum_T_2 : io_B; // @[Alu.scala 67:23]
  wire [31:0] sum = io_A + _sum_T_3; // @[Alu.scala 67:18]
  wire  _cmp_T_7 = io_alu_op[1] ? io_B[31] : io_A[31]; // @[Alu.scala 69:65]
  wire  cmp = io_A[31] == io_B[31] ? sum[31] : _cmp_T_7; // @[Alu.scala 69:8]
  wire [4:0] shamt = io_B[4:0]; // @[Alu.scala 70:19]
  wire [31:0] _GEN_0 = {{16'd0}, io_A[31:16]}; // @[Bitwise.scala 105:31]
  wire [31:0] _shin_T_4 = _GEN_0 & 32'hffff; // @[Bitwise.scala 105:31]
  wire [31:0] _shin_T_6 = {io_A[15:0], 16'h0}; // @[Bitwise.scala 105:70]
  wire [31:0] _shin_T_8 = _shin_T_6 & 32'hffff0000; // @[Bitwise.scala 105:80]
  wire [31:0] _shin_T_9 = _shin_T_4 | _shin_T_8; // @[Bitwise.scala 105:39]
  wire [31:0] _GEN_1 = {{8'd0}, _shin_T_9[31:8]}; // @[Bitwise.scala 105:31]
  wire [31:0] _shin_T_14 = _GEN_1 & 32'hff00ff; // @[Bitwise.scala 105:31]
  wire [31:0] _shin_T_16 = {_shin_T_9[23:0], 8'h0}; // @[Bitwise.scala 105:70]
  wire [31:0] _shin_T_18 = _shin_T_16 & 32'hff00ff00; // @[Bitwise.scala 105:80]
  wire [31:0] _shin_T_19 = _shin_T_14 | _shin_T_18; // @[Bitwise.scala 105:39]
  wire [31:0] _GEN_2 = {{4'd0}, _shin_T_19[31:4]}; // @[Bitwise.scala 105:31]
  wire [31:0] _shin_T_24 = _GEN_2 & 32'hf0f0f0f; // @[Bitwise.scala 105:31]
  wire [31:0] _shin_T_26 = {_shin_T_19[27:0], 4'h0}; // @[Bitwise.scala 105:70]
  wire [31:0] _shin_T_28 = _shin_T_26 & 32'hf0f0f0f0; // @[Bitwise.scala 105:80]
  wire [31:0] _shin_T_29 = _shin_T_24 | _shin_T_28; // @[Bitwise.scala 105:39]
  wire [31:0] _GEN_3 = {{2'd0}, _shin_T_29[31:2]}; // @[Bitwise.scala 105:31]
  wire [31:0] _shin_T_34 = _GEN_3 & 32'h33333333; // @[Bitwise.scala 105:31]
  wire [31:0] _shin_T_36 = {_shin_T_29[29:0], 2'h0}; // @[Bitwise.scala 105:70]
  wire [31:0] _shin_T_38 = _shin_T_36 & 32'hcccccccc; // @[Bitwise.scala 105:80]
  wire [31:0] _shin_T_39 = _shin_T_34 | _shin_T_38; // @[Bitwise.scala 105:39]
  wire [31:0] _GEN_4 = {{1'd0}, _shin_T_39[31:1]}; // @[Bitwise.scala 105:31]
  wire [31:0] _shin_T_44 = _GEN_4 & 32'h55555555; // @[Bitwise.scala 105:31]
  wire [31:0] _shin_T_46 = {_shin_T_39[30:0], 1'h0}; // @[Bitwise.scala 105:70]
  wire [31:0] _shin_T_48 = _shin_T_46 & 32'haaaaaaaa; // @[Bitwise.scala 105:80]
  wire [31:0] _shin_T_49 = _shin_T_44 | _shin_T_48; // @[Bitwise.scala 105:39]
  wire [31:0] shin = io_alu_op[3] ? io_A : _shin_T_49; // @[Alu.scala 71:17]
  wire  _shiftr_T_2 = io_alu_op[0] & shin[31]; // @[Alu.scala 72:34]
  wire [32:0] _shiftr_T_4 = {_shiftr_T_2,shin}; // @[Alu.scala 72:60]
  wire [32:0] _shiftr_T_5 = $signed(_shiftr_T_4) >>> shamt; // @[Alu.scala 72:67]
  wire [31:0] shiftr = _shiftr_T_5[31:0]; // @[Alu.scala 72:76]
  wire [31:0] _GEN_5 = {{16'd0}, shiftr[31:16]}; // @[Bitwise.scala 105:31]
  wire [31:0] _shiftl_T_3 = _GEN_5 & 32'hffff; // @[Bitwise.scala 105:31]
  wire [31:0] _shiftl_T_5 = {shiftr[15:0], 16'h0}; // @[Bitwise.scala 105:70]
  wire [31:0] _shiftl_T_7 = _shiftl_T_5 & 32'hffff0000; // @[Bitwise.scala 105:80]
  wire [31:0] _shiftl_T_8 = _shiftl_T_3 | _shiftl_T_7; // @[Bitwise.scala 105:39]
  wire [31:0] _GEN_6 = {{8'd0}, _shiftl_T_8[31:8]}; // @[Bitwise.scala 105:31]
  wire [31:0] _shiftl_T_13 = _GEN_6 & 32'hff00ff; // @[Bitwise.scala 105:31]
  wire [31:0] _shiftl_T_15 = {_shiftl_T_8[23:0], 8'h0}; // @[Bitwise.scala 105:70]
  wire [31:0] _shiftl_T_17 = _shiftl_T_15 & 32'hff00ff00; // @[Bitwise.scala 105:80]
  wire [31:0] _shiftl_T_18 = _shiftl_T_13 | _shiftl_T_17; // @[Bitwise.scala 105:39]
  wire [31:0] _GEN_7 = {{4'd0}, _shiftl_T_18[31:4]}; // @[Bitwise.scala 105:31]
  wire [31:0] _shiftl_T_23 = _GEN_7 & 32'hf0f0f0f; // @[Bitwise.scala 105:31]
  wire [31:0] _shiftl_T_25 = {_shiftl_T_18[27:0], 4'h0}; // @[Bitwise.scala 105:70]
  wire [31:0] _shiftl_T_27 = _shiftl_T_25 & 32'hf0f0f0f0; // @[Bitwise.scala 105:80]
  wire [31:0] _shiftl_T_28 = _shiftl_T_23 | _shiftl_T_27; // @[Bitwise.scala 105:39]
  wire [31:0] _GEN_8 = {{2'd0}, _shiftl_T_28[31:2]}; // @[Bitwise.scala 105:31]
  wire [31:0] _shiftl_T_33 = _GEN_8 & 32'h33333333; // @[Bitwise.scala 105:31]
  wire [31:0] _shiftl_T_35 = {_shiftl_T_28[29:0], 2'h0}; // @[Bitwise.scala 105:70]
  wire [31:0] _shiftl_T_37 = _shiftl_T_35 & 32'hcccccccc; // @[Bitwise.scala 105:80]
  wire [31:0] _shiftl_T_38 = _shiftl_T_33 | _shiftl_T_37; // @[Bitwise.scala 105:39]
  wire [31:0] _GEN_9 = {{1'd0}, _shiftl_T_38[31:1]}; // @[Bitwise.scala 105:31]
  wire [31:0] _shiftl_T_43 = _GEN_9 & 32'h55555555; // @[Bitwise.scala 105:31]
  wire [31:0] _shiftl_T_45 = {_shiftl_T_38[30:0], 1'h0}; // @[Bitwise.scala 105:70]
  wire [31:0] _shiftl_T_47 = _shiftl_T_45 & 32'haaaaaaaa; // @[Bitwise.scala 105:80]
  wire [31:0] shiftl = _shiftl_T_43 | _shiftl_T_47; // @[Bitwise.scala 105:39]
  wire  _out_T_2 = io_alu_op == 4'h0 | io_alu_op == 4'h1; // @[Alu.scala 77:29]
  wire  _out_T_5 = io_alu_op == 4'h5 | io_alu_op == 4'h7; // @[Alu.scala 80:31]
  wire  _out_T_8 = io_alu_op == 4'h9 | io_alu_op == 4'h8; // @[Alu.scala 83:33]
  wire  _out_T_9 = io_alu_op == 4'h6; // @[Alu.scala 86:23]
  wire  _out_T_10 = io_alu_op == 4'h2; // @[Alu.scala 89:25]
  wire [31:0] _out_T_11 = io_A & io_B; // @[Alu.scala 90:20]
  wire  _out_T_12 = io_alu_op == 4'h3; // @[Alu.scala 92:27]
  wire [31:0] _out_T_13 = io_A | io_B; // @[Alu.scala 93:22]
  wire [31:0] _out_T_15 = io_A ^ io_B; // @[Alu.scala 94:49]
  wire [31:0] _out_T_17 = io_alu_op == 4'ha ? io_A : io_B; // @[Alu.scala 94:60]
  wire [31:0] _out_T_18 = io_alu_op == 4'h4 ? _out_T_15 : _out_T_17; // @[Alu.scala 94:20]
  wire [31:0] _out_T_19 = _out_T_12 ? _out_T_13 : _out_T_18; // @[Alu.scala 91:18]
  wire [31:0] _out_T_20 = _out_T_10 ? _out_T_11 : _out_T_19; // @[Alu.scala 88:16]
  wire [31:0] _out_T_21 = _out_T_9 ? shiftl : _out_T_20; // @[Alu.scala 85:14]
  wire [31:0] _out_T_22 = _out_T_8 ? shiftr : _out_T_21; // @[Alu.scala 82:12]
  wire [31:0] _out_T_23 = _out_T_5 ? {{31'd0}, cmp} : _out_T_22; // @[Alu.scala 79:10]
  assign io_out = _out_T_2 ? sum : _out_T_23; // @[Alu.scala 76:8]
  assign io_sum = io_A + _sum_T_3; // @[Alu.scala 67:18]
endmodule
module ImmGenWire(
  input  [31:0] io_inst,
  input  [2:0]  io_sel,
  output [31:0] io_out
);
  wire [11:0] Iimm = io_inst[31:20]; // @[ImmGen.scala 22:30]
  wire [11:0] Simm = {io_inst[31:25],io_inst[11:7]}; // @[ImmGen.scala 23:51]
  wire [12:0] Bimm = {io_inst[31],io_inst[7],io_inst[30:25],io_inst[11:8],1'h0}; // @[ImmGen.scala 24:86]
  wire [31:0] Uimm = {io_inst[31:12],12'h0}; // @[ImmGen.scala 25:46]
  wire [20:0] Jimm = {io_inst[31],io_inst[19:12],io_inst[20],io_inst[30:25],io_inst[24:21],1'h0}; // @[ImmGen.scala 26:105]
  wire [5:0] Zimm = {1'b0,$signed(io_inst[19:15])}; // @[ImmGen.scala 27:30]
  wire [11:0] _io_out_T_1 = $signed(Iimm) & -12'sh2; // @[ImmGen.scala 31:10]
  wire [11:0] _io_out_T_3 = 3'h1 == io_sel ? $signed(Iimm) : $signed(_io_out_T_1); // @[Mux.scala 81:58]
  wire [11:0] _io_out_T_5 = 3'h2 == io_sel ? $signed(Simm) : $signed(_io_out_T_3); // @[Mux.scala 81:58]
  wire [12:0] _io_out_T_7 = 3'h5 == io_sel ? $signed(Bimm) : $signed({{1{_io_out_T_5[11]}},_io_out_T_5}); // @[Mux.scala 81:58]
  wire [31:0] _io_out_T_9 = 3'h3 == io_sel ? $signed(Uimm) : $signed({{19{_io_out_T_7[12]}},_io_out_T_7}); // @[Mux.scala 81:58]
  wire [31:0] _io_out_T_11 = 3'h4 == io_sel ? $signed({{11{Jimm[20]}},Jimm}) : $signed(_io_out_T_9); // @[Mux.scala 81:58]
  assign io_out = 3'h6 == io_sel ? $signed({{26{Zimm[5]}},Zimm}) : $signed(_io_out_T_11); // @[ImmGen.scala 33:5]
endmodule
module BrCondArea(
  input  [31:0] io_rs1,
  input  [31:0] io_rs2,
  input  [2:0]  io_br_type,
  output        io_taken
);
  wire [31:0] diff = io_rs1 - io_rs2; // @[BrCond.scala 39:21]
  wire  neq = |diff; // @[BrCond.scala 40:18]
  wire  eq = ~neq; // @[BrCond.scala 41:12]
  wire  isSameSign = io_rs1[31] == io_rs2[31]; // @[BrCond.scala 42:37]
  wire  lt = isSameSign ? diff[31] : io_rs1[31]; // @[BrCond.scala 43:15]
  wire  ltu = isSameSign ? diff[31] : io_rs2[31]; // @[BrCond.scala 44:16]
  wire  ge = ~lt; // @[BrCond.scala 45:12]
  wire  geu = ~ltu; // @[BrCond.scala 46:13]
  wire  _io_taken_T_3 = io_br_type == 3'h6 & neq; // @[BrCond.scala 49:31]
  wire  _io_taken_T_4 = io_br_type == 3'h3 & eq | _io_taken_T_3; // @[BrCond.scala 48:36]
  wire  _io_taken_T_6 = io_br_type == 3'h2 & lt; // @[BrCond.scala 50:31]
  wire  _io_taken_T_7 = _io_taken_T_4 | _io_taken_T_6; // @[BrCond.scala 49:39]
  wire  _io_taken_T_9 = io_br_type == 3'h5 & ge; // @[BrCond.scala 51:31]
  wire  _io_taken_T_10 = _io_taken_T_7 | _io_taken_T_9; // @[BrCond.scala 50:38]
  wire  _io_taken_T_12 = io_br_type == 3'h1 & ltu; // @[BrCond.scala 52:32]
  wire  _io_taken_T_13 = _io_taken_T_10 | _io_taken_T_12; // @[BrCond.scala 51:38]
  wire  _io_taken_T_15 = io_br_type == 3'h4 & geu; // @[BrCond.scala 53:32]
  assign io_taken = _io_taken_T_13 | _io_taken_T_15; // @[BrCond.scala 52:40]
endmodule
module Datapath(
  input         clock,
  input         reset,
  input         io_host_fromhost_valid,
  input  [31:0] io_host_fromhost_bits,
  output [31:0] io_host_tohost,
  input         io_icache_resp_valid,
  input  [31:0] io_icache_resp_bits_data,
  output        io_dcache_abort,
  input         io_dcache_resp_valid,
  input  [31:0] io_dcache_resp_bits_data,
  output        io_iaxi2apb_req_valid,
  output [31:0] io_iaxi2apb_req_bits_addr,
  input         io_iaxi2apb_resp_valid,
  input  [31:0] io_iaxi2apb_resp_bits_data,
  output        io_daxi2apb_req_valid,
  output [31:0] io_daxi2apb_req_bits_addr,
  output [31:0] io_daxi2apb_req_bits_data,
  output [3:0]  io_daxi2apb_req_bits_mask,
  input         io_daxi2apb_resp_valid,
  input  [31:0] io_daxi2apb_resp_bits_data,
  input         io_uart_resp_valid,
  output [31:0] io_ctrl_inst,
  input  [1:0]  io_ctrl_pc_sel,
  input         io_ctrl_inst_kill,
  input         io_ctrl_A_sel,
  input         io_ctrl_B_sel,
  input  [2:0]  io_ctrl_imm_sel,
  input  [3:0]  io_ctrl_alu_op,
  input  [2:0]  io_ctrl_br_type,
  input  [1:0]  io_ctrl_st_type,
  input  [2:0]  io_ctrl_ld_type,
  input  [1:0]  io_ctrl_wb_sel,
  input         io_ctrl_wb_en,
  input  [2:0]  io_ctrl_csr_cmd,
  input         io_ctrl_illegal
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
  reg [31:0] _RAND_1;
  reg [31:0] _RAND_2;
  reg [31:0] _RAND_3;
  reg [31:0] _RAND_4;
  reg [31:0] _RAND_5;
  reg [31:0] _RAND_6;
  reg [31:0] _RAND_7;
  reg [31:0] _RAND_8;
  reg [31:0] _RAND_9;
  reg [31:0] _RAND_10;
  reg [31:0] _RAND_11;
  reg [31:0] _RAND_12;
  reg [31:0] _RAND_13;
  reg [63:0] _RAND_14;
`endif // RANDOMIZE_REG_INIT
  wire  csr_clock; // @[Datapath.scala 55:19]
  wire  csr_reset; // @[Datapath.scala 55:19]
  wire  csr_io_stall; // @[Datapath.scala 55:19]
  wire [2:0] csr_io_cmd; // @[Datapath.scala 55:19]
  wire [31:0] csr_io_in; // @[Datapath.scala 55:19]
  wire [31:0] csr_io_out; // @[Datapath.scala 55:19]
  wire [31:0] csr_io_pc; // @[Datapath.scala 55:19]
  wire [31:0] csr_io_addr; // @[Datapath.scala 55:19]
  wire [31:0] csr_io_inst; // @[Datapath.scala 55:19]
  wire  csr_io_illegal; // @[Datapath.scala 55:19]
  wire [1:0] csr_io_st_type; // @[Datapath.scala 55:19]
  wire [2:0] csr_io_ld_type; // @[Datapath.scala 55:19]
  wire  csr_io_pc_check; // @[Datapath.scala 55:19]
  wire  csr_io_expt; // @[Datapath.scala 55:19]
  wire [31:0] csr_io_evec; // @[Datapath.scala 55:19]
  wire [31:0] csr_io_epc; // @[Datapath.scala 55:19]
  wire  csr_io_in_valid; // @[Datapath.scala 55:19]
  wire [63:0] csr_io_in_mtimecmp; // @[Datapath.scala 55:19]
  wire  csr_io_host_fromhost_valid; // @[Datapath.scala 55:19]
  wire [31:0] csr_io_host_fromhost_bits; // @[Datapath.scala 55:19]
  wire [31:0] csr_io_host_tohost; // @[Datapath.scala 55:19]
  wire  regFile_clock; // @[Datapath.scala 56:23]
  wire [4:0] regFile_io_raddr1; // @[Datapath.scala 56:23]
  wire [4:0] regFile_io_raddr2; // @[Datapath.scala 56:23]
  wire [31:0] regFile_io_rdata1; // @[Datapath.scala 56:23]
  wire [31:0] regFile_io_rdata2; // @[Datapath.scala 56:23]
  wire  regFile_io_wen; // @[Datapath.scala 56:23]
  wire [4:0] regFile_io_waddr; // @[Datapath.scala 56:23]
  wire [31:0] regFile_io_wdata; // @[Datapath.scala 56:23]
  wire [31:0] alu_io_A; // @[Datapath.scala 57:19]
  wire [31:0] alu_io_B; // @[Datapath.scala 57:19]
  wire [3:0] alu_io_alu_op; // @[Datapath.scala 57:19]
  wire [31:0] alu_io_out; // @[Datapath.scala 57:19]
  wire [31:0] alu_io_sum; // @[Datapath.scala 57:19]
  wire [31:0] immGen_io_inst; // @[Datapath.scala 58:22]
  wire [2:0] immGen_io_sel; // @[Datapath.scala 58:22]
  wire [31:0] immGen_io_out; // @[Datapath.scala 58:22]
  wire [31:0] brCond_io_rs1; // @[Datapath.scala 59:22]
  wire [31:0] brCond_io_rs2; // @[Datapath.scala 59:22]
  wire [2:0] brCond_io_br_type; // @[Datapath.scala 59:22]
  wire  brCond_io_taken; // @[Datapath.scala 59:22]
  reg [31:0] fe_reg_inst; // @[Datapath.scala 67:23]
  reg [31:0] fe_reg_pc; // @[Datapath.scala 67:23]
  reg [31:0] ew_reg_inst; // @[Datapath.scala 76:23]
  reg [31:0] ew_reg_pc; // @[Datapath.scala 76:23]
  reg [31:0] ew_reg_alu; // @[Datapath.scala 76:23]
  reg [31:0] ew_reg_csr_in; // @[Datapath.scala 76:23]
  reg [1:0] st_type; // @[Datapath.scala 87:20]
  reg [2:0] ld_type; // @[Datapath.scala 91:20]
  reg [1:0] wb_sel; // @[Datapath.scala 92:19]
  reg  wb_en; // @[Datapath.scala 93:18]
  reg [2:0] csr_cmd; // @[Datapath.scala 94:20]
  reg  illegal; // @[Datapath.scala 95:20]
  reg  pc_check; // @[Datapath.scala 96:21]
  reg  started; // @[Datapath.scala 101:24]
  wire  _stall_T_3 = ~io_iaxi2apb_resp_valid; // @[Datapath.scala 103:17]
  wire  _stall_T_4 = ~io_icache_resp_valid | ~io_dcache_resp_valid | _stall_T_3; // @[Datapath.scala 102:62]
  wire  stall = _stall_T_4 | ~io_daxi2apb_resp_valid | ~io_uart_resp_valid; // @[Datapath.scala 103:68]
  wire [31:0] _pc_T_1 = 32'h30000000 - 32'h4; // @[Datapath.scala 104:50]
  reg [32:0] pc; // @[Datapath.scala 104:19]
  wire [32:0] _next_pc_T_1 = pc + 33'h4; // @[Datapath.scala 107:8]
  wire  _next_pc_T_2 = stall | io_daxi2apb_req_valid; // @[Datapath.scala 109:14]
  wire  _next_pc_T_3 = io_ctrl_pc_sel == 2'h3; // @[Datapath.scala 111:23]
  wire  _next_pc_T_4 = io_ctrl_pc_sel == 2'h1; // @[Datapath.scala 112:24]
  wire  _next_pc_T_5 = io_ctrl_pc_sel == 2'h1 | brCond_io_taken; // @[Datapath.scala 112:36]
  wire [31:0] _next_pc_T_6 = {{1'd0}, alu_io_sum[31:1]}; // @[Datapath.scala 112:73]
  wire [32:0] _next_pc_T_7 = {_next_pc_T_6, 1'h0}; // @[Datapath.scala 112:80]
  wire  _next_pc_T_8 = io_ctrl_pc_sel == 2'h2; // @[Datapath.scala 113:23]
  wire [32:0] _next_pc_T_9 = _next_pc_T_8 ? pc : _next_pc_T_1; // @[Mux.scala 101:16]
  wire [32:0] _next_pc_T_10 = _next_pc_T_5 ? _next_pc_T_7 : _next_pc_T_9; // @[Mux.scala 101:16]
  wire [32:0] _next_pc_T_11 = _next_pc_T_3 ? {{1'd0}, csr_io_epc} : _next_pc_T_10; // @[Mux.scala 101:16]
  wire [32:0] _next_pc_T_12 = csr_io_expt ? {{1'd0}, csr_io_evec} : _next_pc_T_11; // @[Mux.scala 101:16]
  wire [32:0] next_pc = _next_pc_T_2 ? pc : _next_pc_T_12; // @[Mux.scala 101:16]
  wire  flash_mode = next_pc[31:28] == 4'h3; // @[Datapath.scala 117:37]
  wire [31:0] _inst_T_4 = io_iaxi2apb_resp_bits_data == 32'h0 ? 32'h13 : io_iaxi2apb_resp_bits_data; // @[Datapath.scala 123:26]
  wire [31:0] daddrT = stall ? ew_reg_alu : alu_io_sum; // @[Datapath.scala 126:19]
  wire  clint_en = daddrT[31:16] == 16'h200; // @[Datapath.scala 127:34]
  wire  _io_iaxi2apb_req_valid_T = ~stall; // @[Datapath.scala 142:28]
  wire [32:0] _GEN_0 = _io_iaxi2apb_req_valid_T ? pc : {{1'd0}, fe_reg_pc}; // @[Datapath.scala 146:16 147:15 67:23]
  wire [4:0] rs1_addr = fe_reg_inst[19:15]; // @[Datapath.scala 157:29]
  wire [4:0] rs2_addr = fe_reg_inst[24:20]; // @[Datapath.scala 158:29]
  wire [4:0] wb_rd_addr = ew_reg_inst[11:7]; // @[Datapath.scala 167:31]
  wire  rs1hazard = wb_en & |rs1_addr & rs1_addr == wb_rd_addr; // @[Datapath.scala 168:41]
  wire  rs2hazard = wb_en & |rs2_addr & rs2_addr == wb_rd_addr; // @[Datapath.scala 169:41]
  wire  _rs1_T = wb_sel == 2'h1; // @[Datapath.scala 173:24]
  wire  _rs1_T_2 = wb_sel == 2'h0; // @[Datapath.scala 173:78]
  wire [31:0] _rs1_T_4 = wb_sel == 2'h0 & rs1hazard ? ew_reg_alu : regFile_io_rdata1; // @[Datapath.scala 173:70]
  wire [31:0] rs1 = wb_sel == 2'h1 & rs1hazard ? regFile_io_wdata : _rs1_T_4; // @[Datapath.scala 173:16]
  wire [31:0] _rs2_T_4 = _rs1_T_2 & rs2hazard ? ew_reg_alu : regFile_io_rdata2; // @[Datapath.scala 174:70]
  wire [31:0] rs2 = _rs1_T & rs2hazard ? regFile_io_wdata : _rs2_T_4; // @[Datapath.scala 174:16]
  wire  _dcache_en_T_2 = |io_ctrl_st_type | |io_ctrl_ld_type; // @[Datapath.scala 193:52]
  wire [4:0] _GEN_28 = {alu_io_sum[1], 4'h0}; // @[Datapath.scala 196:32]
  wire [7:0] _woffset_T_1 = {{3'd0}, _GEN_28}; // @[Datapath.scala 196:32]
  wire [3:0] _woffset_T_3 = {alu_io_sum[0], 3'h0}; // @[Datapath.scala 196:64]
  wire [7:0] _GEN_29 = {{4'd0}, _woffset_T_3}; // @[Datapath.scala 196:47]
  wire [7:0] woffset = _woffset_T_1 | _GEN_29; // @[Datapath.scala 196:47]
  wire [286:0] _GEN_13 = {{255'd0}, rs2}; // @[Datapath.scala 214:36]
  wire [286:0] _io_daxi2apb_req_bits_data_T = _GEN_13 << woffset; // @[Datapath.scala 214:36]
  wire [1:0] _io_daxi2apb_req_bits_mask_T = stall ? st_type : io_ctrl_st_type; // @[Datapath.scala 216:8]
  wire [4:0] _io_daxi2apb_req_bits_mask_T_2 = 5'h3 << alu_io_sum[1:0]; // @[Datapath.scala 218:47]
  wire [3:0] _io_daxi2apb_req_bits_mask_T_4 = 4'h1 << alu_io_sum[1:0]; // @[Datapath.scala 218:86]
  wire [3:0] _io_daxi2apb_req_bits_mask_T_6 = 2'h1 == _io_daxi2apb_req_bits_mask_T ? 4'hf : 4'h0; // @[Mux.scala 81:58]
  wire [4:0] _io_daxi2apb_req_bits_mask_T_8 = 2'h2 == _io_daxi2apb_req_bits_mask_T ? _io_daxi2apb_req_bits_mask_T_2 : {{
    1'd0}, _io_daxi2apb_req_bits_mask_T_6}; // @[Mux.scala 81:58]
  wire [4:0] _io_daxi2apb_req_bits_mask_T_10 = 2'h3 == _io_daxi2apb_req_bits_mask_T ? {{1'd0},
    _io_daxi2apb_req_bits_mask_T_4} : _io_daxi2apb_req_bits_mask_T_8; // @[Mux.scala 81:58]
  wire  _T_6 = ~csr_io_expt; // @[Datapath.scala 251:24]
  wire [4:0] _GEN_30 = {ew_reg_alu[1], 4'h0}; // @[Datapath.scala 266:32]
  wire [7:0] _loffset_T_1 = {{3'd0}, _GEN_30}; // @[Datapath.scala 266:32]
  wire [3:0] _loffset_T_3 = {ew_reg_alu[0], 3'h0}; // @[Datapath.scala 266:64]
  wire [7:0] _GEN_31 = {{4'd0}, _loffset_T_3}; // @[Datapath.scala 266:47]
  wire [7:0] loffset = _loffset_T_1 | _GEN_31; // @[Datapath.scala 266:47]
  wire [31:0] lshift = io_daxi2apb_resp_bits_data >> loffset; // @[Datapath.scala 273:43]
  wire [32:0] _load_T = {1'b0,$signed(io_daxi2apb_resp_bits_data)}; // @[Datapath.scala 278:32]
  wire [15:0] _load_T_2 = lshift[15:0]; // @[Datapath.scala 280:30]
  wire [7:0] _load_T_4 = lshift[7:0]; // @[Datapath.scala 281:29]
  wire [16:0] _load_T_6 = {1'b0,$signed(lshift[15:0])}; // @[Datapath.scala 282:31]
  wire [8:0] _load_T_8 = {1'b0,$signed(lshift[7:0])}; // @[Datapath.scala 283:30]
  wire [32:0] _load_T_10 = 3'h2 == ld_type ? $signed({{17{_load_T_2[15]}},_load_T_2}) : $signed(_load_T); // @[Mux.scala 81:58]
  wire [32:0] _load_T_12 = 3'h3 == ld_type ? $signed({{25{_load_T_4[7]}},_load_T_4}) : $signed(_load_T_10); // @[Mux.scala 81:58]
  wire [32:0] _load_T_14 = 3'h4 == ld_type ? $signed({{16{_load_T_6[16]}},_load_T_6}) : $signed(_load_T_12); // @[Mux.scala 81:58]
  wire [32:0] load = 3'h5 == ld_type ? $signed({{24{_load_T_8[8]}},_load_T_8}) : $signed(_load_T_14); // @[Mux.scala 81:58]
  wire [32:0] _regWrite_T = {1'b0,$signed(ew_reg_alu)}; // @[Datapath.scala 306:18]
  wire [31:0] _regWrite_T_2 = ew_reg_pc + 32'h4; // @[Datapath.scala 307:48]
  wire [32:0] _regWrite_T_3 = {1'b0,$signed(_regWrite_T_2)}; // @[Datapath.scala 307:55]
  wire [32:0] _regWrite_T_4 = {1'b0,$signed(csr_io_out)}; // @[Datapath.scala 307:82]
  wire [32:0] _regWrite_T_6 = 2'h1 == wb_sel ? $signed(load) : $signed(_regWrite_T); // @[Mux.scala 81:58]
  wire [32:0] _regWrite_T_8 = 2'h2 == wb_sel ? $signed(_regWrite_T_3) : $signed(_regWrite_T_6); // @[Mux.scala 81:58]
  wire [32:0] regWrite = 2'h3 == wb_sel ? $signed(_regWrite_T_4) : $signed(_regWrite_T_8); // @[Datapath.scala 308:7]
  wire [32:0] _GEN_32 = reset ? 33'h0 : _GEN_0; // @[Datapath.scala 67:{23,23}]
  CSR csr ( // @[Datapath.scala 55:19]
    .clock(csr_clock),
    .reset(csr_reset),
    .io_stall(csr_io_stall),
    .io_cmd(csr_io_cmd),
    .io_in(csr_io_in),
    .io_out(csr_io_out),
    .io_pc(csr_io_pc),
    .io_addr(csr_io_addr),
    .io_inst(csr_io_inst),
    .io_illegal(csr_io_illegal),
    .io_st_type(csr_io_st_type),
    .io_ld_type(csr_io_ld_type),
    .io_pc_check(csr_io_pc_check),
    .io_expt(csr_io_expt),
    .io_evec(csr_io_evec),
    .io_epc(csr_io_epc),
    .io_in_valid(csr_io_in_valid),
    .io_in_mtimecmp(csr_io_in_mtimecmp),
    .io_host_fromhost_valid(csr_io_host_fromhost_valid),
    .io_host_fromhost_bits(csr_io_host_fromhost_bits),
    .io_host_tohost(csr_io_host_tohost)
  );
  RegFile regFile ( // @[Datapath.scala 56:23]
    .clock(regFile_clock),
    .io_raddr1(regFile_io_raddr1),
    .io_raddr2(regFile_io_raddr2),
    .io_rdata1(regFile_io_rdata1),
    .io_rdata2(regFile_io_rdata2),
    .io_wen(regFile_io_wen),
    .io_waddr(regFile_io_waddr),
    .io_wdata(regFile_io_wdata)
  );
  AluArea alu ( // @[Datapath.scala 57:19]
    .io_A(alu_io_A),
    .io_B(alu_io_B),
    .io_alu_op(alu_io_alu_op),
    .io_out(alu_io_out),
    .io_sum(alu_io_sum)
  );
  ImmGenWire immGen ( // @[Datapath.scala 58:22]
    .io_inst(immGen_io_inst),
    .io_sel(immGen_io_sel),
    .io_out(immGen_io_out)
  );
  BrCondArea brCond ( // @[Datapath.scala 59:22]
    .io_rs1(brCond_io_rs1),
    .io_rs2(brCond_io_rs2),
    .io_br_type(brCond_io_br_type),
    .io_taken(brCond_io_taken)
  );
  assign io_host_tohost = csr_io_host_tohost; // @[Datapath.scala 298:11]
  assign io_dcache_abort = csr_io_expt; // @[Datapath.scala 315:19]
  assign io_iaxi2apb_req_valid = ~stall & ~io_daxi2apb_req_valid; // @[Datapath.scala 142:35]
  assign io_iaxi2apb_req_bits_addr = next_pc[31:0]; // @[Datapath.scala 138:29]
  assign io_daxi2apb_req_valid = _io_iaxi2apb_req_valid_T & _dcache_en_T_2; // @[Datapath.scala 212:35]
  assign io_daxi2apb_req_bits_addr = stall ? ew_reg_alu : alu_io_sum; // @[Datapath.scala 126:19]
  assign io_daxi2apb_req_bits_data = _io_daxi2apb_req_bits_data_T[31:0]; // @[Datapath.scala 214:29]
  assign io_daxi2apb_req_bits_mask = _io_daxi2apb_req_bits_mask_T_10[3:0]; // @[Datapath.scala 215:29]
  assign io_ctrl_inst = fe_reg_inst; // @[Datapath.scala 153:16]
  assign csr_clock = clock;
  assign csr_reset = reset;
  assign csr_io_stall = _stall_T_4 | ~io_daxi2apb_resp_valid | ~io_uart_resp_valid; // @[Datapath.scala 103:68]
  assign csr_io_cmd = csr_cmd; // @[Datapath.scala 290:14]
  assign csr_io_in = ew_reg_csr_in; // @[Datapath.scala 289:13]
  assign csr_io_pc = ew_reg_pc; // @[Datapath.scala 292:13]
  assign csr_io_addr = ew_reg_alu; // @[Datapath.scala 293:15]
  assign csr_io_inst = ew_reg_inst; // @[Datapath.scala 291:15]
  assign csr_io_illegal = illegal; // @[Datapath.scala 294:18]
  assign csr_io_st_type = st_type; // @[Datapath.scala 297:18]
  assign csr_io_ld_type = ld_type; // @[Datapath.scala 296:18]
  assign csr_io_pc_check = pc_check; // @[Datapath.scala 295:19]
  assign csr_io_in_valid = clint_en & io_dcache_resp_valid; // @[Datapath.scala 299:31]
  assign csr_io_in_mtimecmp = {{32'd0}, io_dcache_resp_bits_data}; // @[Datapath.scala 300:22]
  assign csr_io_host_fromhost_valid = io_host_fromhost_valid; // @[Datapath.scala 298:11]
  assign csr_io_host_fromhost_bits = io_host_fromhost_bits; // @[Datapath.scala 298:11]
  assign regFile_clock = clock;
  assign regFile_io_raddr1 = fe_reg_inst[19:15]; // @[Datapath.scala 157:29]
  assign regFile_io_raddr2 = fe_reg_inst[24:20]; // @[Datapath.scala 158:29]
  assign regFile_io_wen = wb_en & _io_iaxi2apb_req_valid_T & _T_6; // @[Datapath.scala 310:37]
  assign regFile_io_waddr = ew_reg_inst[11:7]; // @[Datapath.scala 167:31]
  assign regFile_io_wdata = regWrite[31:0]; // @[Datapath.scala 312:20]
  assign alu_io_A = io_ctrl_A_sel ? rs1 : fe_reg_pc; // @[Datapath.scala 176:18]
  assign alu_io_B = io_ctrl_B_sel ? rs2 : immGen_io_out; // @[Datapath.scala 177:18]
  assign alu_io_alu_op = io_ctrl_alu_op; // @[Datapath.scala 178:17]
  assign immGen_io_inst = fe_reg_inst; // @[Datapath.scala 163:18]
  assign immGen_io_sel = io_ctrl_imm_sel; // @[Datapath.scala 164:17]
  assign brCond_io_rs1 = wb_sel == 2'h1 & rs1hazard ? regFile_io_wdata : _rs1_T_4; // @[Datapath.scala 173:16]
  assign brCond_io_rs2 = _rs1_T & rs2hazard ? regFile_io_wdata : _rs2_T_4; // @[Datapath.scala 174:16]
  assign brCond_io_br_type = io_ctrl_br_type; // @[Datapath.scala 183:21]
  always @(posedge clock) begin
    if (reset) begin // @[Datapath.scala 67:23]
      fe_reg_inst <= 32'h13; // @[Datapath.scala 67:23]
    end else if (_io_iaxi2apb_req_valid_T) begin // @[Datapath.scala 146:16]
      if (started | io_ctrl_inst_kill | brCond_io_taken | csr_io_expt) begin // @[Datapath.scala 122:8]
        fe_reg_inst <= 32'h13;
      end else if (flash_mode) begin // @[Datapath.scala 123:10]
        fe_reg_inst <= _inst_T_4;
      end else begin
        fe_reg_inst <= io_icache_resp_bits_data;
      end
    end
    fe_reg_pc <= _GEN_32[31:0]; // @[Datapath.scala 67:{23,23}]
    if (reset) begin // @[Datapath.scala 76:23]
      ew_reg_inst <= 32'h13; // @[Datapath.scala 76:23]
    end else if (!(reset | _io_iaxi2apb_req_valid_T & csr_io_expt)) begin // @[Datapath.scala 244:47]
      if (_io_iaxi2apb_req_valid_T & ~csr_io_expt) begin // @[Datapath.scala 251:38]
        ew_reg_inst <= fe_reg_inst; // @[Datapath.scala 253:17]
      end
    end
    if (reset) begin // @[Datapath.scala 76:23]
      ew_reg_pc <= 32'h0; // @[Datapath.scala 76:23]
    end else if (!(reset | _io_iaxi2apb_req_valid_T & csr_io_expt)) begin // @[Datapath.scala 244:47]
      if (_io_iaxi2apb_req_valid_T & ~csr_io_expt) begin // @[Datapath.scala 251:38]
        ew_reg_pc <= fe_reg_pc; // @[Datapath.scala 252:15]
      end
    end
    if (reset) begin // @[Datapath.scala 76:23]
      ew_reg_alu <= 32'h0; // @[Datapath.scala 76:23]
    end else if (!(reset | _io_iaxi2apb_req_valid_T & csr_io_expt)) begin // @[Datapath.scala 244:47]
      if (_io_iaxi2apb_req_valid_T & ~csr_io_expt) begin // @[Datapath.scala 251:38]
        ew_reg_alu <= alu_io_out; // @[Datapath.scala 254:16]
      end
    end
    if (reset) begin // @[Datapath.scala 76:23]
      ew_reg_csr_in <= 32'h0; // @[Datapath.scala 76:23]
    end else if (!(reset | _io_iaxi2apb_req_valid_T & csr_io_expt)) begin // @[Datapath.scala 244:47]
      if (_io_iaxi2apb_req_valid_T & ~csr_io_expt) begin // @[Datapath.scala 251:38]
        if (io_ctrl_imm_sel == 3'h6) begin // @[Datapath.scala 255:25]
          ew_reg_csr_in <= immGen_io_out;
        end else begin
          ew_reg_csr_in <= rs1;
        end
      end
    end
    if (reset | _io_iaxi2apb_req_valid_T & csr_io_expt) begin // @[Datapath.scala 244:47]
      st_type <= 2'h0; // @[Datapath.scala 245:13]
    end else if (_io_iaxi2apb_req_valid_T & ~csr_io_expt) begin // @[Datapath.scala 251:38]
      st_type <= io_ctrl_st_type; // @[Datapath.scala 256:13]
    end
    if (reset | _io_iaxi2apb_req_valid_T & csr_io_expt) begin // @[Datapath.scala 244:47]
      ld_type <= 3'h0; // @[Datapath.scala 246:13]
    end else if (_io_iaxi2apb_req_valid_T & ~csr_io_expt) begin // @[Datapath.scala 251:38]
      ld_type <= io_ctrl_ld_type; // @[Datapath.scala 257:13]
    end
    if (!(reset | _io_iaxi2apb_req_valid_T & csr_io_expt)) begin // @[Datapath.scala 244:47]
      if (_io_iaxi2apb_req_valid_T & ~csr_io_expt) begin // @[Datapath.scala 251:38]
        wb_sel <= io_ctrl_wb_sel; // @[Datapath.scala 258:12]
      end
    end
    if (reset | _io_iaxi2apb_req_valid_T & csr_io_expt) begin // @[Datapath.scala 244:47]
      wb_en <= 1'h0; // @[Datapath.scala 247:11]
    end else if (_io_iaxi2apb_req_valid_T & ~csr_io_expt) begin // @[Datapath.scala 251:38]
      wb_en <= io_ctrl_wb_en; // @[Datapath.scala 259:11]
    end
    if (reset | _io_iaxi2apb_req_valid_T & csr_io_expt) begin // @[Datapath.scala 244:47]
      csr_cmd <= 3'h0; // @[Datapath.scala 248:13]
    end else if (_io_iaxi2apb_req_valid_T & ~csr_io_expt) begin // @[Datapath.scala 251:38]
      csr_cmd <= io_ctrl_csr_cmd; // @[Datapath.scala 260:13]
    end
    if (reset | _io_iaxi2apb_req_valid_T & csr_io_expt) begin // @[Datapath.scala 244:47]
      illegal <= 1'h0; // @[Datapath.scala 249:13]
    end else if (_io_iaxi2apb_req_valid_T & ~csr_io_expt) begin // @[Datapath.scala 251:38]
      illegal <= io_ctrl_illegal; // @[Datapath.scala 261:13]
    end
    if (reset | _io_iaxi2apb_req_valid_T & csr_io_expt) begin // @[Datapath.scala 244:47]
      pc_check <= 1'h0; // @[Datapath.scala 250:14]
    end else if (_io_iaxi2apb_req_valid_T & ~csr_io_expt) begin // @[Datapath.scala 251:38]
      pc_check <= _next_pc_T_4; // @[Datapath.scala 262:14]
    end
    started <= reset; // @[Datapath.scala 101:31]
    if (reset) begin // @[Datapath.scala 104:19]
      pc <= {{1'd0}, _pc_T_1}; // @[Datapath.scala 104:19]
    end else if (!(_next_pc_T_2)) begin // @[Mux.scala 101:16]
      if (csr_io_expt) begin // @[Mux.scala 101:16]
        pc <= {{1'd0}, csr_io_evec};
      end else if (_next_pc_T_3) begin // @[Mux.scala 101:16]
        pc <= {{1'd0}, csr_io_epc};
      end else begin
        pc <= _next_pc_T_10;
      end
    end
  end
// Register and memory initialization
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
`ifdef FIRRTL_BEFORE_INITIAL
`FIRRTL_BEFORE_INITIAL
`endif
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
`ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  fe_reg_inst = _RAND_0[31:0];
  _RAND_1 = {1{`RANDOM}};
  fe_reg_pc = _RAND_1[31:0];
  _RAND_2 = {1{`RANDOM}};
  ew_reg_inst = _RAND_2[31:0];
  _RAND_3 = {1{`RANDOM}};
  ew_reg_pc = _RAND_3[31:0];
  _RAND_4 = {1{`RANDOM}};
  ew_reg_alu = _RAND_4[31:0];
  _RAND_5 = {1{`RANDOM}};
  ew_reg_csr_in = _RAND_5[31:0];
  _RAND_6 = {1{`RANDOM}};
  st_type = _RAND_6[1:0];
  _RAND_7 = {1{`RANDOM}};
  ld_type = _RAND_7[2:0];
  _RAND_8 = {1{`RANDOM}};
  wb_sel = _RAND_8[1:0];
  _RAND_9 = {1{`RANDOM}};
  wb_en = _RAND_9[0:0];
  _RAND_10 = {1{`RANDOM}};
  csr_cmd = _RAND_10[2:0];
  _RAND_11 = {1{`RANDOM}};
  illegal = _RAND_11[0:0];
  _RAND_12 = {1{`RANDOM}};
  pc_check = _RAND_12[0:0];
  _RAND_13 = {1{`RANDOM}};
  started = _RAND_13[0:0];
  _RAND_14 = {2{`RANDOM}};
  pc = _RAND_14[32:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
module Control(
  input  [31:0] io_inst,
  output [1:0]  io_pc_sel,
  output        io_inst_kill,
  output        io_A_sel,
  output        io_B_sel,
  output [2:0]  io_imm_sel,
  output [3:0]  io_alu_op,
  output [2:0]  io_br_type,
  output [1:0]  io_st_type,
  output [2:0]  io_ld_type,
  output [1:0]  io_wb_sel,
  output        io_wb_en,
  output [2:0]  io_csr_cmd,
  output        io_illegal
);
  wire [31:0] _ctrlSignals_T = io_inst & 32'h7f; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_1 = 32'h37 == _ctrlSignals_T; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_3 = 32'h17 == _ctrlSignals_T; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_5 = 32'h6f == _ctrlSignals_T; // @[Lookup.scala 31:38]
  wire [31:0] _ctrlSignals_T_6 = io_inst & 32'h707f; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_7 = 32'h67 == _ctrlSignals_T_6; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_9 = 32'h63 == _ctrlSignals_T_6; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_11 = 32'h1063 == _ctrlSignals_T_6; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_13 = 32'h4063 == _ctrlSignals_T_6; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_15 = 32'h5063 == _ctrlSignals_T_6; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_17 = 32'h6063 == _ctrlSignals_T_6; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_19 = 32'h7063 == _ctrlSignals_T_6; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_21 = 32'h3 == _ctrlSignals_T_6; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_23 = 32'h1003 == _ctrlSignals_T_6; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_25 = 32'h2003 == _ctrlSignals_T_6; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_27 = 32'h4003 == _ctrlSignals_T_6; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_29 = 32'h5003 == _ctrlSignals_T_6; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_31 = 32'h23 == _ctrlSignals_T_6; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_33 = 32'h1023 == _ctrlSignals_T_6; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_35 = 32'h2023 == _ctrlSignals_T_6; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_37 = 32'h13 == _ctrlSignals_T_6; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_39 = 32'h2013 == _ctrlSignals_T_6; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_41 = 32'h3013 == _ctrlSignals_T_6; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_43 = 32'h4013 == _ctrlSignals_T_6; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_45 = 32'h6013 == _ctrlSignals_T_6; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_47 = 32'h7013 == _ctrlSignals_T_6; // @[Lookup.scala 31:38]
  wire [31:0] _ctrlSignals_T_48 = io_inst & 32'hfe00707f; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_49 = 32'h1013 == _ctrlSignals_T_48; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_51 = 32'h5013 == _ctrlSignals_T_48; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_53 = 32'h40005013 == _ctrlSignals_T_48; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_55 = 32'h33 == _ctrlSignals_T_48; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_57 = 32'h40000033 == _ctrlSignals_T_48; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_59 = 32'h1033 == _ctrlSignals_T_48; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_61 = 32'h2033 == _ctrlSignals_T_48; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_63 = 32'h3033 == _ctrlSignals_T_48; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_65 = 32'h4033 == _ctrlSignals_T_48; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_67 = 32'h5033 == _ctrlSignals_T_48; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_69 = 32'h40005033 == _ctrlSignals_T_48; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_71 = 32'h6033 == _ctrlSignals_T_48; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_73 = 32'h7033 == _ctrlSignals_T_48; // @[Lookup.scala 31:38]
  wire [31:0] _ctrlSignals_T_74 = io_inst & 32'hf00fffff; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_75 = 32'hf == _ctrlSignals_T_74; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_77 = 32'h100f == io_inst; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_79 = 32'h1073 == _ctrlSignals_T_6; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_81 = 32'h2073 == _ctrlSignals_T_6; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_83 = 32'h3073 == _ctrlSignals_T_6; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_85 = 32'h5073 == _ctrlSignals_T_6; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_87 = 32'h6073 == _ctrlSignals_T_6; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_89 = 32'h7073 == _ctrlSignals_T_6; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_91 = 32'h73 == io_inst; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_93 = 32'h100073 == io_inst; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_95 = 32'h10000073 == io_inst; // @[Lookup.scala 31:38]
  wire  _ctrlSignals_T_97 = 32'h10200073 == io_inst; // @[Lookup.scala 31:38]
  wire [1:0] _ctrlSignals_T_99 = _ctrlSignals_T_95 ? 2'h3 : 2'h0; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_100 = _ctrlSignals_T_93 ? 2'h0 : _ctrlSignals_T_99; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_101 = _ctrlSignals_T_91 ? 2'h0 : _ctrlSignals_T_100; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_102 = _ctrlSignals_T_89 ? 2'h2 : _ctrlSignals_T_101; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_103 = _ctrlSignals_T_87 ? 2'h2 : _ctrlSignals_T_102; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_104 = _ctrlSignals_T_85 ? 2'h2 : _ctrlSignals_T_103; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_105 = _ctrlSignals_T_83 ? 2'h2 : _ctrlSignals_T_104; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_106 = _ctrlSignals_T_81 ? 2'h2 : _ctrlSignals_T_105; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_107 = _ctrlSignals_T_79 ? 2'h2 : _ctrlSignals_T_106; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_108 = _ctrlSignals_T_77 ? 2'h2 : _ctrlSignals_T_107; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_109 = _ctrlSignals_T_75 ? 2'h0 : _ctrlSignals_T_108; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_110 = _ctrlSignals_T_73 ? 2'h0 : _ctrlSignals_T_109; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_111 = _ctrlSignals_T_71 ? 2'h0 : _ctrlSignals_T_110; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_112 = _ctrlSignals_T_69 ? 2'h0 : _ctrlSignals_T_111; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_113 = _ctrlSignals_T_67 ? 2'h0 : _ctrlSignals_T_112; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_114 = _ctrlSignals_T_65 ? 2'h0 : _ctrlSignals_T_113; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_115 = _ctrlSignals_T_63 ? 2'h0 : _ctrlSignals_T_114; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_116 = _ctrlSignals_T_61 ? 2'h0 : _ctrlSignals_T_115; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_117 = _ctrlSignals_T_59 ? 2'h0 : _ctrlSignals_T_116; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_118 = _ctrlSignals_T_57 ? 2'h0 : _ctrlSignals_T_117; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_119 = _ctrlSignals_T_55 ? 2'h0 : _ctrlSignals_T_118; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_120 = _ctrlSignals_T_53 ? 2'h0 : _ctrlSignals_T_119; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_121 = _ctrlSignals_T_51 ? 2'h0 : _ctrlSignals_T_120; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_122 = _ctrlSignals_T_49 ? 2'h0 : _ctrlSignals_T_121; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_123 = _ctrlSignals_T_47 ? 2'h0 : _ctrlSignals_T_122; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_124 = _ctrlSignals_T_45 ? 2'h0 : _ctrlSignals_T_123; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_125 = _ctrlSignals_T_43 ? 2'h0 : _ctrlSignals_T_124; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_126 = _ctrlSignals_T_41 ? 2'h0 : _ctrlSignals_T_125; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_127 = _ctrlSignals_T_39 ? 2'h0 : _ctrlSignals_T_126; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_128 = _ctrlSignals_T_37 ? 2'h0 : _ctrlSignals_T_127; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_129 = _ctrlSignals_T_35 ? 2'h0 : _ctrlSignals_T_128; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_130 = _ctrlSignals_T_33 ? 2'h0 : _ctrlSignals_T_129; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_131 = _ctrlSignals_T_31 ? 2'h0 : _ctrlSignals_T_130; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_132 = _ctrlSignals_T_29 ? 2'h2 : _ctrlSignals_T_131; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_133 = _ctrlSignals_T_27 ? 2'h2 : _ctrlSignals_T_132; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_134 = _ctrlSignals_T_25 ? 2'h2 : _ctrlSignals_T_133; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_135 = _ctrlSignals_T_23 ? 2'h2 : _ctrlSignals_T_134; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_136 = _ctrlSignals_T_21 ? 2'h2 : _ctrlSignals_T_135; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_137 = _ctrlSignals_T_19 ? 2'h0 : _ctrlSignals_T_136; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_138 = _ctrlSignals_T_17 ? 2'h0 : _ctrlSignals_T_137; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_139 = _ctrlSignals_T_15 ? 2'h0 : _ctrlSignals_T_138; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_140 = _ctrlSignals_T_13 ? 2'h0 : _ctrlSignals_T_139; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_141 = _ctrlSignals_T_11 ? 2'h0 : _ctrlSignals_T_140; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_142 = _ctrlSignals_T_9 ? 2'h0 : _ctrlSignals_T_141; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_143 = _ctrlSignals_T_7 ? 2'h1 : _ctrlSignals_T_142; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_144 = _ctrlSignals_T_5 ? 2'h1 : _ctrlSignals_T_143; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_145 = _ctrlSignals_T_3 ? 2'h0 : _ctrlSignals_T_144; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_156 = _ctrlSignals_T_77 ? 1'h0 : _ctrlSignals_T_79 | (_ctrlSignals_T_81 | _ctrlSignals_T_83); // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_157 = _ctrlSignals_T_75 ? 1'h0 : _ctrlSignals_T_156; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_185 = _ctrlSignals_T_19 ? 1'h0 : _ctrlSignals_T_21 | (_ctrlSignals_T_23 | (_ctrlSignals_T_25 | (
    _ctrlSignals_T_27 | (_ctrlSignals_T_29 | (_ctrlSignals_T_31 | (_ctrlSignals_T_33 | (_ctrlSignals_T_35 | (
    _ctrlSignals_T_37 | (_ctrlSignals_T_39 | (_ctrlSignals_T_41 | (_ctrlSignals_T_43 | (_ctrlSignals_T_45 | (
    _ctrlSignals_T_47 | (_ctrlSignals_T_49 | (_ctrlSignals_T_51 | (_ctrlSignals_T_53 | (_ctrlSignals_T_55 | (
    _ctrlSignals_T_57 | (_ctrlSignals_T_59 | (_ctrlSignals_T_61 | (_ctrlSignals_T_63 | (_ctrlSignals_T_65 | (
    _ctrlSignals_T_67 | (_ctrlSignals_T_69 | (_ctrlSignals_T_71 | (_ctrlSignals_T_73 | _ctrlSignals_T_157)))))))))))))))
    ))))))))))); // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_186 = _ctrlSignals_T_17 ? 1'h0 : _ctrlSignals_T_185; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_187 = _ctrlSignals_T_15 ? 1'h0 : _ctrlSignals_T_186; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_188 = _ctrlSignals_T_13 ? 1'h0 : _ctrlSignals_T_187; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_189 = _ctrlSignals_T_11 ? 1'h0 : _ctrlSignals_T_188; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_190 = _ctrlSignals_T_9 ? 1'h0 : _ctrlSignals_T_189; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_192 = _ctrlSignals_T_5 ? 1'h0 : _ctrlSignals_T_7 | _ctrlSignals_T_190; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_193 = _ctrlSignals_T_3 ? 1'h0 : _ctrlSignals_T_192; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_216 = _ctrlSignals_T_53 ? 1'h0 : _ctrlSignals_T_55 | (_ctrlSignals_T_57 | (_ctrlSignals_T_59 | (
    _ctrlSignals_T_61 | (_ctrlSignals_T_63 | (_ctrlSignals_T_65 | (_ctrlSignals_T_67 | (_ctrlSignals_T_69 | (
    _ctrlSignals_T_71 | _ctrlSignals_T_73)))))))); // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_217 = _ctrlSignals_T_51 ? 1'h0 : _ctrlSignals_T_216; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_218 = _ctrlSignals_T_49 ? 1'h0 : _ctrlSignals_T_217; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_219 = _ctrlSignals_T_47 ? 1'h0 : _ctrlSignals_T_218; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_220 = _ctrlSignals_T_45 ? 1'h0 : _ctrlSignals_T_219; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_221 = _ctrlSignals_T_43 ? 1'h0 : _ctrlSignals_T_220; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_222 = _ctrlSignals_T_41 ? 1'h0 : _ctrlSignals_T_221; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_223 = _ctrlSignals_T_39 ? 1'h0 : _ctrlSignals_T_222; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_224 = _ctrlSignals_T_37 ? 1'h0 : _ctrlSignals_T_223; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_225 = _ctrlSignals_T_35 ? 1'h0 : _ctrlSignals_T_224; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_226 = _ctrlSignals_T_33 ? 1'h0 : _ctrlSignals_T_225; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_227 = _ctrlSignals_T_31 ? 1'h0 : _ctrlSignals_T_226; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_228 = _ctrlSignals_T_29 ? 1'h0 : _ctrlSignals_T_227; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_229 = _ctrlSignals_T_27 ? 1'h0 : _ctrlSignals_T_228; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_230 = _ctrlSignals_T_25 ? 1'h0 : _ctrlSignals_T_229; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_231 = _ctrlSignals_T_23 ? 1'h0 : _ctrlSignals_T_230; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_232 = _ctrlSignals_T_21 ? 1'h0 : _ctrlSignals_T_231; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_233 = _ctrlSignals_T_19 ? 1'h0 : _ctrlSignals_T_232; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_234 = _ctrlSignals_T_17 ? 1'h0 : _ctrlSignals_T_233; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_235 = _ctrlSignals_T_15 ? 1'h0 : _ctrlSignals_T_234; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_236 = _ctrlSignals_T_13 ? 1'h0 : _ctrlSignals_T_235; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_237 = _ctrlSignals_T_11 ? 1'h0 : _ctrlSignals_T_236; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_238 = _ctrlSignals_T_9 ? 1'h0 : _ctrlSignals_T_237; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_239 = _ctrlSignals_T_7 ? 1'h0 : _ctrlSignals_T_238; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_240 = _ctrlSignals_T_5 ? 1'h0 : _ctrlSignals_T_239; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_241 = _ctrlSignals_T_3 ? 1'h0 : _ctrlSignals_T_240; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_246 = _ctrlSignals_T_89 ? 3'h6 : 3'h0; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_247 = _ctrlSignals_T_87 ? 3'h6 : _ctrlSignals_T_246; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_248 = _ctrlSignals_T_85 ? 3'h6 : _ctrlSignals_T_247; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_249 = _ctrlSignals_T_83 ? 3'h0 : _ctrlSignals_T_248; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_250 = _ctrlSignals_T_81 ? 3'h0 : _ctrlSignals_T_249; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_251 = _ctrlSignals_T_79 ? 3'h0 : _ctrlSignals_T_250; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_252 = _ctrlSignals_T_77 ? 3'h0 : _ctrlSignals_T_251; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_253 = _ctrlSignals_T_75 ? 3'h0 : _ctrlSignals_T_252; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_254 = _ctrlSignals_T_73 ? 3'h0 : _ctrlSignals_T_253; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_255 = _ctrlSignals_T_71 ? 3'h0 : _ctrlSignals_T_254; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_256 = _ctrlSignals_T_69 ? 3'h0 : _ctrlSignals_T_255; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_257 = _ctrlSignals_T_67 ? 3'h0 : _ctrlSignals_T_256; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_258 = _ctrlSignals_T_65 ? 3'h0 : _ctrlSignals_T_257; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_259 = _ctrlSignals_T_63 ? 3'h0 : _ctrlSignals_T_258; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_260 = _ctrlSignals_T_61 ? 3'h0 : _ctrlSignals_T_259; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_261 = _ctrlSignals_T_59 ? 3'h0 : _ctrlSignals_T_260; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_262 = _ctrlSignals_T_57 ? 3'h0 : _ctrlSignals_T_261; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_263 = _ctrlSignals_T_55 ? 3'h0 : _ctrlSignals_T_262; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_264 = _ctrlSignals_T_53 ? 3'h1 : _ctrlSignals_T_263; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_265 = _ctrlSignals_T_51 ? 3'h1 : _ctrlSignals_T_264; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_266 = _ctrlSignals_T_49 ? 3'h1 : _ctrlSignals_T_265; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_267 = _ctrlSignals_T_47 ? 3'h1 : _ctrlSignals_T_266; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_268 = _ctrlSignals_T_45 ? 3'h1 : _ctrlSignals_T_267; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_269 = _ctrlSignals_T_43 ? 3'h1 : _ctrlSignals_T_268; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_270 = _ctrlSignals_T_41 ? 3'h1 : _ctrlSignals_T_269; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_271 = _ctrlSignals_T_39 ? 3'h1 : _ctrlSignals_T_270; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_272 = _ctrlSignals_T_37 ? 3'h1 : _ctrlSignals_T_271; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_273 = _ctrlSignals_T_35 ? 3'h2 : _ctrlSignals_T_272; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_274 = _ctrlSignals_T_33 ? 3'h2 : _ctrlSignals_T_273; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_275 = _ctrlSignals_T_31 ? 3'h2 : _ctrlSignals_T_274; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_276 = _ctrlSignals_T_29 ? 3'h1 : _ctrlSignals_T_275; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_277 = _ctrlSignals_T_27 ? 3'h1 : _ctrlSignals_T_276; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_278 = _ctrlSignals_T_25 ? 3'h1 : _ctrlSignals_T_277; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_279 = _ctrlSignals_T_23 ? 3'h1 : _ctrlSignals_T_278; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_280 = _ctrlSignals_T_21 ? 3'h1 : _ctrlSignals_T_279; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_281 = _ctrlSignals_T_19 ? 3'h5 : _ctrlSignals_T_280; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_282 = _ctrlSignals_T_17 ? 3'h5 : _ctrlSignals_T_281; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_283 = _ctrlSignals_T_15 ? 3'h5 : _ctrlSignals_T_282; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_284 = _ctrlSignals_T_13 ? 3'h5 : _ctrlSignals_T_283; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_285 = _ctrlSignals_T_11 ? 3'h5 : _ctrlSignals_T_284; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_286 = _ctrlSignals_T_9 ? 3'h5 : _ctrlSignals_T_285; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_287 = _ctrlSignals_T_7 ? 3'h1 : _ctrlSignals_T_286; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_288 = _ctrlSignals_T_5 ? 3'h4 : _ctrlSignals_T_287; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_289 = _ctrlSignals_T_3 ? 3'h3 : _ctrlSignals_T_288; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_297 = _ctrlSignals_T_83 ? 4'ha : 4'hf; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_298 = _ctrlSignals_T_81 ? 4'ha : _ctrlSignals_T_297; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_299 = _ctrlSignals_T_79 ? 4'ha : _ctrlSignals_T_298; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_300 = _ctrlSignals_T_77 ? 4'hf : _ctrlSignals_T_299; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_301 = _ctrlSignals_T_75 ? 4'hf : _ctrlSignals_T_300; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_302 = _ctrlSignals_T_73 ? 4'h2 : _ctrlSignals_T_301; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_303 = _ctrlSignals_T_71 ? 4'h3 : _ctrlSignals_T_302; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_304 = _ctrlSignals_T_69 ? 4'h9 : _ctrlSignals_T_303; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_305 = _ctrlSignals_T_67 ? 4'h8 : _ctrlSignals_T_304; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_306 = _ctrlSignals_T_65 ? 4'h4 : _ctrlSignals_T_305; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_307 = _ctrlSignals_T_63 ? 4'h7 : _ctrlSignals_T_306; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_308 = _ctrlSignals_T_61 ? 4'h5 : _ctrlSignals_T_307; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_309 = _ctrlSignals_T_59 ? 4'h6 : _ctrlSignals_T_308; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_310 = _ctrlSignals_T_57 ? 4'h1 : _ctrlSignals_T_309; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_311 = _ctrlSignals_T_55 ? 4'h0 : _ctrlSignals_T_310; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_312 = _ctrlSignals_T_53 ? 4'h9 : _ctrlSignals_T_311; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_313 = _ctrlSignals_T_51 ? 4'h8 : _ctrlSignals_T_312; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_314 = _ctrlSignals_T_49 ? 4'h6 : _ctrlSignals_T_313; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_315 = _ctrlSignals_T_47 ? 4'h2 : _ctrlSignals_T_314; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_316 = _ctrlSignals_T_45 ? 4'h3 : _ctrlSignals_T_315; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_317 = _ctrlSignals_T_43 ? 4'h4 : _ctrlSignals_T_316; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_318 = _ctrlSignals_T_41 ? 4'h7 : _ctrlSignals_T_317; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_319 = _ctrlSignals_T_39 ? 4'h5 : _ctrlSignals_T_318; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_320 = _ctrlSignals_T_37 ? 4'h0 : _ctrlSignals_T_319; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_321 = _ctrlSignals_T_35 ? 4'h0 : _ctrlSignals_T_320; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_322 = _ctrlSignals_T_33 ? 4'h0 : _ctrlSignals_T_321; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_323 = _ctrlSignals_T_31 ? 4'h0 : _ctrlSignals_T_322; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_324 = _ctrlSignals_T_29 ? 4'h0 : _ctrlSignals_T_323; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_325 = _ctrlSignals_T_27 ? 4'h0 : _ctrlSignals_T_324; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_326 = _ctrlSignals_T_25 ? 4'h0 : _ctrlSignals_T_325; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_327 = _ctrlSignals_T_23 ? 4'h0 : _ctrlSignals_T_326; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_328 = _ctrlSignals_T_21 ? 4'h0 : _ctrlSignals_T_327; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_329 = _ctrlSignals_T_19 ? 4'h0 : _ctrlSignals_T_328; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_330 = _ctrlSignals_T_17 ? 4'h0 : _ctrlSignals_T_329; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_331 = _ctrlSignals_T_15 ? 4'h0 : _ctrlSignals_T_330; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_332 = _ctrlSignals_T_13 ? 4'h0 : _ctrlSignals_T_331; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_333 = _ctrlSignals_T_11 ? 4'h0 : _ctrlSignals_T_332; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_334 = _ctrlSignals_T_9 ? 4'h0 : _ctrlSignals_T_333; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_335 = _ctrlSignals_T_7 ? 4'h0 : _ctrlSignals_T_334; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_336 = _ctrlSignals_T_5 ? 4'h0 : _ctrlSignals_T_335; // @[Lookup.scala 34:39]
  wire [3:0] _ctrlSignals_T_337 = _ctrlSignals_T_3 ? 4'h0 : _ctrlSignals_T_336; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_377 = _ctrlSignals_T_19 ? 3'h4 : 3'h0; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_378 = _ctrlSignals_T_17 ? 3'h1 : _ctrlSignals_T_377; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_379 = _ctrlSignals_T_15 ? 3'h5 : _ctrlSignals_T_378; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_380 = _ctrlSignals_T_13 ? 3'h2 : _ctrlSignals_T_379; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_381 = _ctrlSignals_T_11 ? 3'h6 : _ctrlSignals_T_380; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_382 = _ctrlSignals_T_9 ? 3'h3 : _ctrlSignals_T_381; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_383 = _ctrlSignals_T_7 ? 3'h0 : _ctrlSignals_T_382; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_384 = _ctrlSignals_T_5 ? 3'h0 : _ctrlSignals_T_383; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_385 = _ctrlSignals_T_3 ? 3'h0 : _ctrlSignals_T_384; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_388 = _ctrlSignals_T_93 ? 1'h0 : _ctrlSignals_T_95; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_389 = _ctrlSignals_T_91 ? 1'h0 : _ctrlSignals_T_388; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_397 = _ctrlSignals_T_75 ? 1'h0 : _ctrlSignals_T_77 | (_ctrlSignals_T_79 | (_ctrlSignals_T_81 | (
    _ctrlSignals_T_83 | (_ctrlSignals_T_85 | (_ctrlSignals_T_87 | (_ctrlSignals_T_89 | _ctrlSignals_T_389)))))); // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_398 = _ctrlSignals_T_73 ? 1'h0 : _ctrlSignals_T_397; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_399 = _ctrlSignals_T_71 ? 1'h0 : _ctrlSignals_T_398; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_400 = _ctrlSignals_T_69 ? 1'h0 : _ctrlSignals_T_399; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_401 = _ctrlSignals_T_67 ? 1'h0 : _ctrlSignals_T_400; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_402 = _ctrlSignals_T_65 ? 1'h0 : _ctrlSignals_T_401; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_403 = _ctrlSignals_T_63 ? 1'h0 : _ctrlSignals_T_402; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_404 = _ctrlSignals_T_61 ? 1'h0 : _ctrlSignals_T_403; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_405 = _ctrlSignals_T_59 ? 1'h0 : _ctrlSignals_T_404; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_406 = _ctrlSignals_T_57 ? 1'h0 : _ctrlSignals_T_405; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_407 = _ctrlSignals_T_55 ? 1'h0 : _ctrlSignals_T_406; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_408 = _ctrlSignals_T_53 ? 1'h0 : _ctrlSignals_T_407; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_409 = _ctrlSignals_T_51 ? 1'h0 : _ctrlSignals_T_408; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_410 = _ctrlSignals_T_49 ? 1'h0 : _ctrlSignals_T_409; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_411 = _ctrlSignals_T_47 ? 1'h0 : _ctrlSignals_T_410; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_412 = _ctrlSignals_T_45 ? 1'h0 : _ctrlSignals_T_411; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_413 = _ctrlSignals_T_43 ? 1'h0 : _ctrlSignals_T_412; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_414 = _ctrlSignals_T_41 ? 1'h0 : _ctrlSignals_T_413; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_415 = _ctrlSignals_T_39 ? 1'h0 : _ctrlSignals_T_414; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_416 = _ctrlSignals_T_37 ? 1'h0 : _ctrlSignals_T_415; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_417 = _ctrlSignals_T_35 ? 1'h0 : _ctrlSignals_T_416; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_418 = _ctrlSignals_T_33 ? 1'h0 : _ctrlSignals_T_417; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_419 = _ctrlSignals_T_31 ? 1'h0 : _ctrlSignals_T_418; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_420 = _ctrlSignals_T_29 ? 1'h0 : _ctrlSignals_T_419; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_421 = _ctrlSignals_T_27 ? 1'h0 : _ctrlSignals_T_420; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_422 = _ctrlSignals_T_25 ? 1'h0 : _ctrlSignals_T_421; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_423 = _ctrlSignals_T_23 ? 1'h0 : _ctrlSignals_T_422; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_424 = _ctrlSignals_T_21 ? 1'h0 : _ctrlSignals_T_423; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_425 = _ctrlSignals_T_19 ? 1'h0 : _ctrlSignals_T_424; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_426 = _ctrlSignals_T_17 ? 1'h0 : _ctrlSignals_T_425; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_427 = _ctrlSignals_T_15 ? 1'h0 : _ctrlSignals_T_426; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_428 = _ctrlSignals_T_13 ? 1'h0 : _ctrlSignals_T_427; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_429 = _ctrlSignals_T_11 ? 1'h0 : _ctrlSignals_T_428; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_430 = _ctrlSignals_T_9 ? 1'h0 : _ctrlSignals_T_429; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_433 = _ctrlSignals_T_3 ? 1'h0 : _ctrlSignals_T_5 | (_ctrlSignals_T_7 | _ctrlSignals_T_430); // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_465 = _ctrlSignals_T_35 ? 2'h1 : 2'h0; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_466 = _ctrlSignals_T_33 ? 2'h2 : _ctrlSignals_T_465; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_467 = _ctrlSignals_T_31 ? 2'h3 : _ctrlSignals_T_466; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_468 = _ctrlSignals_T_29 ? 2'h0 : _ctrlSignals_T_467; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_469 = _ctrlSignals_T_27 ? 2'h0 : _ctrlSignals_T_468; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_470 = _ctrlSignals_T_25 ? 2'h0 : _ctrlSignals_T_469; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_471 = _ctrlSignals_T_23 ? 2'h0 : _ctrlSignals_T_470; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_472 = _ctrlSignals_T_21 ? 2'h0 : _ctrlSignals_T_471; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_473 = _ctrlSignals_T_19 ? 2'h0 : _ctrlSignals_T_472; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_474 = _ctrlSignals_T_17 ? 2'h0 : _ctrlSignals_T_473; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_475 = _ctrlSignals_T_15 ? 2'h0 : _ctrlSignals_T_474; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_476 = _ctrlSignals_T_13 ? 2'h0 : _ctrlSignals_T_475; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_477 = _ctrlSignals_T_11 ? 2'h0 : _ctrlSignals_T_476; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_478 = _ctrlSignals_T_9 ? 2'h0 : _ctrlSignals_T_477; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_479 = _ctrlSignals_T_7 ? 2'h0 : _ctrlSignals_T_478; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_480 = _ctrlSignals_T_5 ? 2'h0 : _ctrlSignals_T_479; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_481 = _ctrlSignals_T_3 ? 2'h0 : _ctrlSignals_T_480; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_516 = _ctrlSignals_T_29 ? 3'h4 : 3'h0; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_517 = _ctrlSignals_T_27 ? 3'h5 : _ctrlSignals_T_516; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_518 = _ctrlSignals_T_25 ? 3'h1 : _ctrlSignals_T_517; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_519 = _ctrlSignals_T_23 ? 3'h2 : _ctrlSignals_T_518; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_520 = _ctrlSignals_T_21 ? 3'h3 : _ctrlSignals_T_519; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_521 = _ctrlSignals_T_19 ? 3'h0 : _ctrlSignals_T_520; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_522 = _ctrlSignals_T_17 ? 3'h0 : _ctrlSignals_T_521; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_523 = _ctrlSignals_T_15 ? 3'h0 : _ctrlSignals_T_522; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_524 = _ctrlSignals_T_13 ? 3'h0 : _ctrlSignals_T_523; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_525 = _ctrlSignals_T_11 ? 3'h0 : _ctrlSignals_T_524; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_526 = _ctrlSignals_T_9 ? 3'h0 : _ctrlSignals_T_525; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_527 = _ctrlSignals_T_7 ? 3'h0 : _ctrlSignals_T_526; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_528 = _ctrlSignals_T_5 ? 3'h0 : _ctrlSignals_T_527; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_529 = _ctrlSignals_T_3 ? 3'h0 : _ctrlSignals_T_528; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_532 = _ctrlSignals_T_93 ? 2'h3 : _ctrlSignals_T_99; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_533 = _ctrlSignals_T_91 ? 2'h3 : _ctrlSignals_T_532; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_534 = _ctrlSignals_T_89 ? 2'h3 : _ctrlSignals_T_533; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_535 = _ctrlSignals_T_87 ? 2'h3 : _ctrlSignals_T_534; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_536 = _ctrlSignals_T_85 ? 2'h3 : _ctrlSignals_T_535; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_537 = _ctrlSignals_T_83 ? 2'h3 : _ctrlSignals_T_536; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_538 = _ctrlSignals_T_81 ? 2'h3 : _ctrlSignals_T_537; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_539 = _ctrlSignals_T_79 ? 2'h3 : _ctrlSignals_T_538; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_540 = _ctrlSignals_T_77 ? 2'h0 : _ctrlSignals_T_539; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_541 = _ctrlSignals_T_75 ? 2'h0 : _ctrlSignals_T_540; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_542 = _ctrlSignals_T_73 ? 2'h0 : _ctrlSignals_T_541; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_543 = _ctrlSignals_T_71 ? 2'h0 : _ctrlSignals_T_542; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_544 = _ctrlSignals_T_69 ? 2'h0 : _ctrlSignals_T_543; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_545 = _ctrlSignals_T_67 ? 2'h0 : _ctrlSignals_T_544; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_546 = _ctrlSignals_T_65 ? 2'h0 : _ctrlSignals_T_545; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_547 = _ctrlSignals_T_63 ? 2'h0 : _ctrlSignals_T_546; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_548 = _ctrlSignals_T_61 ? 2'h0 : _ctrlSignals_T_547; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_549 = _ctrlSignals_T_59 ? 2'h0 : _ctrlSignals_T_548; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_550 = _ctrlSignals_T_57 ? 2'h0 : _ctrlSignals_T_549; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_551 = _ctrlSignals_T_55 ? 2'h0 : _ctrlSignals_T_550; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_552 = _ctrlSignals_T_53 ? 2'h0 : _ctrlSignals_T_551; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_553 = _ctrlSignals_T_51 ? 2'h0 : _ctrlSignals_T_552; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_554 = _ctrlSignals_T_49 ? 2'h0 : _ctrlSignals_T_553; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_555 = _ctrlSignals_T_47 ? 2'h0 : _ctrlSignals_T_554; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_556 = _ctrlSignals_T_45 ? 2'h0 : _ctrlSignals_T_555; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_557 = _ctrlSignals_T_43 ? 2'h0 : _ctrlSignals_T_556; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_558 = _ctrlSignals_T_41 ? 2'h0 : _ctrlSignals_T_557; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_559 = _ctrlSignals_T_39 ? 2'h0 : _ctrlSignals_T_558; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_560 = _ctrlSignals_T_37 ? 2'h0 : _ctrlSignals_T_559; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_561 = _ctrlSignals_T_35 ? 2'h0 : _ctrlSignals_T_560; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_562 = _ctrlSignals_T_33 ? 2'h0 : _ctrlSignals_T_561; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_563 = _ctrlSignals_T_31 ? 2'h0 : _ctrlSignals_T_562; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_564 = _ctrlSignals_T_29 ? 2'h1 : _ctrlSignals_T_563; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_565 = _ctrlSignals_T_27 ? 2'h1 : _ctrlSignals_T_564; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_566 = _ctrlSignals_T_25 ? 2'h1 : _ctrlSignals_T_565; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_567 = _ctrlSignals_T_23 ? 2'h1 : _ctrlSignals_T_566; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_568 = _ctrlSignals_T_21 ? 2'h1 : _ctrlSignals_T_567; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_569 = _ctrlSignals_T_19 ? 2'h0 : _ctrlSignals_T_568; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_570 = _ctrlSignals_T_17 ? 2'h0 : _ctrlSignals_T_569; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_571 = _ctrlSignals_T_15 ? 2'h0 : _ctrlSignals_T_570; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_572 = _ctrlSignals_T_13 ? 2'h0 : _ctrlSignals_T_571; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_573 = _ctrlSignals_T_11 ? 2'h0 : _ctrlSignals_T_572; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_574 = _ctrlSignals_T_9 ? 2'h0 : _ctrlSignals_T_573; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_575 = _ctrlSignals_T_7 ? 2'h2 : _ctrlSignals_T_574; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_576 = _ctrlSignals_T_5 ? 2'h2 : _ctrlSignals_T_575; // @[Lookup.scala 34:39]
  wire [1:0] _ctrlSignals_T_577 = _ctrlSignals_T_3 ? 2'h0 : _ctrlSignals_T_576; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_588 = _ctrlSignals_T_77 ? 1'h0 : _ctrlSignals_T_79 | (_ctrlSignals_T_81 | (_ctrlSignals_T_83 | (
    _ctrlSignals_T_85 | (_ctrlSignals_T_87 | _ctrlSignals_T_89)))); // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_589 = _ctrlSignals_T_75 ? 1'h0 : _ctrlSignals_T_588; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_609 = _ctrlSignals_T_35 ? 1'h0 : _ctrlSignals_T_37 | (_ctrlSignals_T_39 | (_ctrlSignals_T_41 | (
    _ctrlSignals_T_43 | (_ctrlSignals_T_45 | (_ctrlSignals_T_47 | (_ctrlSignals_T_49 | (_ctrlSignals_T_51 | (
    _ctrlSignals_T_53 | (_ctrlSignals_T_55 | (_ctrlSignals_T_57 | (_ctrlSignals_T_59 | (_ctrlSignals_T_61 | (
    _ctrlSignals_T_63 | (_ctrlSignals_T_65 | (_ctrlSignals_T_67 | (_ctrlSignals_T_69 | (_ctrlSignals_T_71 | (
    _ctrlSignals_T_73 | _ctrlSignals_T_589)))))))))))))))))); // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_610 = _ctrlSignals_T_33 ? 1'h0 : _ctrlSignals_T_609; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_611 = _ctrlSignals_T_31 ? 1'h0 : _ctrlSignals_T_610; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_617 = _ctrlSignals_T_19 ? 1'h0 : _ctrlSignals_T_21 | (_ctrlSignals_T_23 | (_ctrlSignals_T_25 | (
    _ctrlSignals_T_27 | (_ctrlSignals_T_29 | _ctrlSignals_T_611)))); // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_618 = _ctrlSignals_T_17 ? 1'h0 : _ctrlSignals_T_617; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_619 = _ctrlSignals_T_15 ? 1'h0 : _ctrlSignals_T_618; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_620 = _ctrlSignals_T_13 ? 1'h0 : _ctrlSignals_T_619; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_621 = _ctrlSignals_T_11 ? 1'h0 : _ctrlSignals_T_620; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_622 = _ctrlSignals_T_9 ? 1'h0 : _ctrlSignals_T_621; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_627 = _ctrlSignals_T_95 ? 3'h4 : 3'h0; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_628 = _ctrlSignals_T_93 ? 3'h4 : _ctrlSignals_T_627; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_629 = _ctrlSignals_T_91 ? 3'h4 : _ctrlSignals_T_628; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_630 = _ctrlSignals_T_89 ? 3'h3 : _ctrlSignals_T_629; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_631 = _ctrlSignals_T_87 ? 3'h2 : _ctrlSignals_T_630; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_632 = _ctrlSignals_T_85 ? 3'h1 : _ctrlSignals_T_631; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_633 = _ctrlSignals_T_83 ? 3'h3 : _ctrlSignals_T_632; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_634 = _ctrlSignals_T_81 ? 3'h2 : _ctrlSignals_T_633; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_635 = _ctrlSignals_T_79 ? 3'h1 : _ctrlSignals_T_634; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_636 = _ctrlSignals_T_77 ? 3'h0 : _ctrlSignals_T_635; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_637 = _ctrlSignals_T_75 ? 3'h0 : _ctrlSignals_T_636; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_638 = _ctrlSignals_T_73 ? 3'h0 : _ctrlSignals_T_637; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_639 = _ctrlSignals_T_71 ? 3'h0 : _ctrlSignals_T_638; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_640 = _ctrlSignals_T_69 ? 3'h0 : _ctrlSignals_T_639; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_641 = _ctrlSignals_T_67 ? 3'h0 : _ctrlSignals_T_640; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_642 = _ctrlSignals_T_65 ? 3'h0 : _ctrlSignals_T_641; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_643 = _ctrlSignals_T_63 ? 3'h0 : _ctrlSignals_T_642; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_644 = _ctrlSignals_T_61 ? 3'h0 : _ctrlSignals_T_643; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_645 = _ctrlSignals_T_59 ? 3'h0 : _ctrlSignals_T_644; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_646 = _ctrlSignals_T_57 ? 3'h0 : _ctrlSignals_T_645; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_647 = _ctrlSignals_T_55 ? 3'h0 : _ctrlSignals_T_646; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_648 = _ctrlSignals_T_53 ? 3'h0 : _ctrlSignals_T_647; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_649 = _ctrlSignals_T_51 ? 3'h0 : _ctrlSignals_T_648; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_650 = _ctrlSignals_T_49 ? 3'h0 : _ctrlSignals_T_649; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_651 = _ctrlSignals_T_47 ? 3'h0 : _ctrlSignals_T_650; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_652 = _ctrlSignals_T_45 ? 3'h0 : _ctrlSignals_T_651; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_653 = _ctrlSignals_T_43 ? 3'h0 : _ctrlSignals_T_652; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_654 = _ctrlSignals_T_41 ? 3'h0 : _ctrlSignals_T_653; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_655 = _ctrlSignals_T_39 ? 3'h0 : _ctrlSignals_T_654; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_656 = _ctrlSignals_T_37 ? 3'h0 : _ctrlSignals_T_655; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_657 = _ctrlSignals_T_35 ? 3'h0 : _ctrlSignals_T_656; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_658 = _ctrlSignals_T_33 ? 3'h0 : _ctrlSignals_T_657; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_659 = _ctrlSignals_T_31 ? 3'h0 : _ctrlSignals_T_658; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_660 = _ctrlSignals_T_29 ? 3'h0 : _ctrlSignals_T_659; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_661 = _ctrlSignals_T_27 ? 3'h0 : _ctrlSignals_T_660; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_662 = _ctrlSignals_T_25 ? 3'h0 : _ctrlSignals_T_661; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_663 = _ctrlSignals_T_23 ? 3'h0 : _ctrlSignals_T_662; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_664 = _ctrlSignals_T_21 ? 3'h0 : _ctrlSignals_T_663; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_665 = _ctrlSignals_T_19 ? 3'h0 : _ctrlSignals_T_664; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_666 = _ctrlSignals_T_17 ? 3'h0 : _ctrlSignals_T_665; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_667 = _ctrlSignals_T_15 ? 3'h0 : _ctrlSignals_T_666; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_668 = _ctrlSignals_T_13 ? 3'h0 : _ctrlSignals_T_667; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_669 = _ctrlSignals_T_11 ? 3'h0 : _ctrlSignals_T_668; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_670 = _ctrlSignals_T_9 ? 3'h0 : _ctrlSignals_T_669; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_671 = _ctrlSignals_T_7 ? 3'h0 : _ctrlSignals_T_670; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_672 = _ctrlSignals_T_5 ? 3'h0 : _ctrlSignals_T_671; // @[Lookup.scala 34:39]
  wire [2:0] _ctrlSignals_T_673 = _ctrlSignals_T_3 ? 3'h0 : _ctrlSignals_T_672; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_674 = _ctrlSignals_T_97 ? 1'h0 : 1'h1; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_675 = _ctrlSignals_T_95 ? 1'h0 : _ctrlSignals_T_674; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_676 = _ctrlSignals_T_93 ? 1'h0 : _ctrlSignals_T_675; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_677 = _ctrlSignals_T_91 ? 1'h0 : _ctrlSignals_T_676; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_678 = _ctrlSignals_T_89 ? 1'h0 : _ctrlSignals_T_677; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_679 = _ctrlSignals_T_87 ? 1'h0 : _ctrlSignals_T_678; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_680 = _ctrlSignals_T_85 ? 1'h0 : _ctrlSignals_T_679; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_681 = _ctrlSignals_T_83 ? 1'h0 : _ctrlSignals_T_680; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_682 = _ctrlSignals_T_81 ? 1'h0 : _ctrlSignals_T_681; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_683 = _ctrlSignals_T_79 ? 1'h0 : _ctrlSignals_T_682; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_684 = _ctrlSignals_T_77 ? 1'h0 : _ctrlSignals_T_683; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_685 = _ctrlSignals_T_75 ? 1'h0 : _ctrlSignals_T_684; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_686 = _ctrlSignals_T_73 ? 1'h0 : _ctrlSignals_T_685; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_687 = _ctrlSignals_T_71 ? 1'h0 : _ctrlSignals_T_686; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_688 = _ctrlSignals_T_69 ? 1'h0 : _ctrlSignals_T_687; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_689 = _ctrlSignals_T_67 ? 1'h0 : _ctrlSignals_T_688; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_690 = _ctrlSignals_T_65 ? 1'h0 : _ctrlSignals_T_689; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_691 = _ctrlSignals_T_63 ? 1'h0 : _ctrlSignals_T_690; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_692 = _ctrlSignals_T_61 ? 1'h0 : _ctrlSignals_T_691; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_693 = _ctrlSignals_T_59 ? 1'h0 : _ctrlSignals_T_692; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_694 = _ctrlSignals_T_57 ? 1'h0 : _ctrlSignals_T_693; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_695 = _ctrlSignals_T_55 ? 1'h0 : _ctrlSignals_T_694; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_696 = _ctrlSignals_T_53 ? 1'h0 : _ctrlSignals_T_695; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_697 = _ctrlSignals_T_51 ? 1'h0 : _ctrlSignals_T_696; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_698 = _ctrlSignals_T_49 ? 1'h0 : _ctrlSignals_T_697; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_699 = _ctrlSignals_T_47 ? 1'h0 : _ctrlSignals_T_698; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_700 = _ctrlSignals_T_45 ? 1'h0 : _ctrlSignals_T_699; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_701 = _ctrlSignals_T_43 ? 1'h0 : _ctrlSignals_T_700; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_702 = _ctrlSignals_T_41 ? 1'h0 : _ctrlSignals_T_701; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_703 = _ctrlSignals_T_39 ? 1'h0 : _ctrlSignals_T_702; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_704 = _ctrlSignals_T_37 ? 1'h0 : _ctrlSignals_T_703; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_705 = _ctrlSignals_T_35 ? 1'h0 : _ctrlSignals_T_704; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_706 = _ctrlSignals_T_33 ? 1'h0 : _ctrlSignals_T_705; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_707 = _ctrlSignals_T_31 ? 1'h0 : _ctrlSignals_T_706; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_708 = _ctrlSignals_T_29 ? 1'h0 : _ctrlSignals_T_707; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_709 = _ctrlSignals_T_27 ? 1'h0 : _ctrlSignals_T_708; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_710 = _ctrlSignals_T_25 ? 1'h0 : _ctrlSignals_T_709; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_711 = _ctrlSignals_T_23 ? 1'h0 : _ctrlSignals_T_710; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_712 = _ctrlSignals_T_21 ? 1'h0 : _ctrlSignals_T_711; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_713 = _ctrlSignals_T_19 ? 1'h0 : _ctrlSignals_T_712; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_714 = _ctrlSignals_T_17 ? 1'h0 : _ctrlSignals_T_713; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_715 = _ctrlSignals_T_15 ? 1'h0 : _ctrlSignals_T_714; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_716 = _ctrlSignals_T_13 ? 1'h0 : _ctrlSignals_T_715; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_717 = _ctrlSignals_T_11 ? 1'h0 : _ctrlSignals_T_716; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_718 = _ctrlSignals_T_9 ? 1'h0 : _ctrlSignals_T_717; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_719 = _ctrlSignals_T_7 ? 1'h0 : _ctrlSignals_T_718; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_720 = _ctrlSignals_T_5 ? 1'h0 : _ctrlSignals_T_719; // @[Lookup.scala 34:39]
  wire  _ctrlSignals_T_721 = _ctrlSignals_T_3 ? 1'h0 : _ctrlSignals_T_720; // @[Lookup.scala 34:39]
  assign io_pc_sel = _ctrlSignals_T_1 ? 2'h0 : _ctrlSignals_T_145; // @[Lookup.scala 34:39]
  assign io_inst_kill = _ctrlSignals_T_1 ? 1'h0 : _ctrlSignals_T_433; // @[Lookup.scala 34:39]
  assign io_A_sel = _ctrlSignals_T_1 ? 1'h0 : _ctrlSignals_T_193; // @[Lookup.scala 34:39]
  assign io_B_sel = _ctrlSignals_T_1 ? 1'h0 : _ctrlSignals_T_241; // @[Lookup.scala 34:39]
  assign io_imm_sel = _ctrlSignals_T_1 ? 3'h3 : _ctrlSignals_T_289; // @[Lookup.scala 34:39]
  assign io_alu_op = _ctrlSignals_T_1 ? 4'hb : _ctrlSignals_T_337; // @[Lookup.scala 34:39]
  assign io_br_type = _ctrlSignals_T_1 ? 3'h0 : _ctrlSignals_T_385; // @[Lookup.scala 34:39]
  assign io_st_type = _ctrlSignals_T_1 ? 2'h0 : _ctrlSignals_T_481; // @[Lookup.scala 34:39]
  assign io_ld_type = _ctrlSignals_T_1 ? 3'h0 : _ctrlSignals_T_529; // @[Lookup.scala 34:39]
  assign io_wb_sel = _ctrlSignals_T_1 ? 2'h0 : _ctrlSignals_T_577; // @[Lookup.scala 34:39]
  assign io_wb_en = _ctrlSignals_T_1 | (_ctrlSignals_T_3 | (_ctrlSignals_T_5 | (_ctrlSignals_T_7 | _ctrlSignals_T_622)))
    ; // @[Lookup.scala 34:39]
  assign io_csr_cmd = _ctrlSignals_T_1 ? 3'h0 : _ctrlSignals_T_673; // @[Lookup.scala 34:39]
  assign io_illegal = _ctrlSignals_T_1 ? 1'h0 : _ctrlSignals_T_721; // @[Lookup.scala 34:39]
endmodule
module Core(
  input         clock,
  input         reset,
  input         io_host_fromhost_valid,
  input  [31:0] io_host_fromhost_bits,
  output [31:0] io_host_tohost,
  input         io_icache_resp_valid,
  input  [31:0] io_icache_resp_bits_data,
  output        io_dcache_abort,
  input         io_dcache_resp_valid,
  input  [31:0] io_dcache_resp_bits_data,
  output        io_iaxi2apb_req_valid,
  output [31:0] io_iaxi2apb_req_bits_addr,
  input         io_iaxi2apb_resp_valid,
  input  [31:0] io_iaxi2apb_resp_bits_data,
  output        io_daxi2apb_req_valid,
  output [31:0] io_daxi2apb_req_bits_addr,
  output [31:0] io_daxi2apb_req_bits_data,
  output [3:0]  io_daxi2apb_req_bits_mask,
  input         io_daxi2apb_resp_valid,
  input  [31:0] io_daxi2apb_resp_bits_data,
  input         io_uart_resp_valid
);
  wire  dpath_clock; // @[Core.scala 30:21]
  wire  dpath_reset; // @[Core.scala 30:21]
  wire  dpath_io_host_fromhost_valid; // @[Core.scala 30:21]
  wire [31:0] dpath_io_host_fromhost_bits; // @[Core.scala 30:21]
  wire [31:0] dpath_io_host_tohost; // @[Core.scala 30:21]
  wire  dpath_io_icache_resp_valid; // @[Core.scala 30:21]
  wire [31:0] dpath_io_icache_resp_bits_data; // @[Core.scala 30:21]
  wire  dpath_io_dcache_abort; // @[Core.scala 30:21]
  wire  dpath_io_dcache_resp_valid; // @[Core.scala 30:21]
  wire [31:0] dpath_io_dcache_resp_bits_data; // @[Core.scala 30:21]
  wire  dpath_io_iaxi2apb_req_valid; // @[Core.scala 30:21]
  wire [31:0] dpath_io_iaxi2apb_req_bits_addr; // @[Core.scala 30:21]
  wire  dpath_io_iaxi2apb_resp_valid; // @[Core.scala 30:21]
  wire [31:0] dpath_io_iaxi2apb_resp_bits_data; // @[Core.scala 30:21]
  wire  dpath_io_daxi2apb_req_valid; // @[Core.scala 30:21]
  wire [31:0] dpath_io_daxi2apb_req_bits_addr; // @[Core.scala 30:21]
  wire [31:0] dpath_io_daxi2apb_req_bits_data; // @[Core.scala 30:21]
  wire [3:0] dpath_io_daxi2apb_req_bits_mask; // @[Core.scala 30:21]
  wire  dpath_io_daxi2apb_resp_valid; // @[Core.scala 30:21]
  wire [31:0] dpath_io_daxi2apb_resp_bits_data; // @[Core.scala 30:21]
  wire  dpath_io_uart_resp_valid; // @[Core.scala 30:21]
  wire [31:0] dpath_io_ctrl_inst; // @[Core.scala 30:21]
  wire [1:0] dpath_io_ctrl_pc_sel; // @[Core.scala 30:21]
  wire  dpath_io_ctrl_inst_kill; // @[Core.scala 30:21]
  wire  dpath_io_ctrl_A_sel; // @[Core.scala 30:21]
  wire  dpath_io_ctrl_B_sel; // @[Core.scala 30:21]
  wire [2:0] dpath_io_ctrl_imm_sel; // @[Core.scala 30:21]
  wire [3:0] dpath_io_ctrl_alu_op; // @[Core.scala 30:21]
  wire [2:0] dpath_io_ctrl_br_type; // @[Core.scala 30:21]
  wire [1:0] dpath_io_ctrl_st_type; // @[Core.scala 30:21]
  wire [2:0] dpath_io_ctrl_ld_type; // @[Core.scala 30:21]
  wire [1:0] dpath_io_ctrl_wb_sel; // @[Core.scala 30:21]
  wire  dpath_io_ctrl_wb_en; // @[Core.scala 30:21]
  wire [2:0] dpath_io_ctrl_csr_cmd; // @[Core.scala 30:21]
  wire  dpath_io_ctrl_illegal; // @[Core.scala 30:21]
  wire [31:0] ctrl_io_inst; // @[Core.scala 31:20]
  wire [1:0] ctrl_io_pc_sel; // @[Core.scala 31:20]
  wire  ctrl_io_inst_kill; // @[Core.scala 31:20]
  wire  ctrl_io_A_sel; // @[Core.scala 31:20]
  wire  ctrl_io_B_sel; // @[Core.scala 31:20]
  wire [2:0] ctrl_io_imm_sel; // @[Core.scala 31:20]
  wire [3:0] ctrl_io_alu_op; // @[Core.scala 31:20]
  wire [2:0] ctrl_io_br_type; // @[Core.scala 31:20]
  wire [1:0] ctrl_io_st_type; // @[Core.scala 31:20]
  wire [2:0] ctrl_io_ld_type; // @[Core.scala 31:20]
  wire [1:0] ctrl_io_wb_sel; // @[Core.scala 31:20]
  wire  ctrl_io_wb_en; // @[Core.scala 31:20]
  wire [2:0] ctrl_io_csr_cmd; // @[Core.scala 31:20]
  wire  ctrl_io_illegal; // @[Core.scala 31:20]
  Datapath dpath ( // @[Core.scala 30:21]
    .clock(dpath_clock),
    .reset(dpath_reset),
    .io_host_fromhost_valid(dpath_io_host_fromhost_valid),
    .io_host_fromhost_bits(dpath_io_host_fromhost_bits),
    .io_host_tohost(dpath_io_host_tohost),
    .io_icache_resp_valid(dpath_io_icache_resp_valid),
    .io_icache_resp_bits_data(dpath_io_icache_resp_bits_data),
    .io_dcache_abort(dpath_io_dcache_abort),
    .io_dcache_resp_valid(dpath_io_dcache_resp_valid),
    .io_dcache_resp_bits_data(dpath_io_dcache_resp_bits_data),
    .io_iaxi2apb_req_valid(dpath_io_iaxi2apb_req_valid),
    .io_iaxi2apb_req_bits_addr(dpath_io_iaxi2apb_req_bits_addr),
    .io_iaxi2apb_resp_valid(dpath_io_iaxi2apb_resp_valid),
    .io_iaxi2apb_resp_bits_data(dpath_io_iaxi2apb_resp_bits_data),
    .io_daxi2apb_req_valid(dpath_io_daxi2apb_req_valid),
    .io_daxi2apb_req_bits_addr(dpath_io_daxi2apb_req_bits_addr),
    .io_daxi2apb_req_bits_data(dpath_io_daxi2apb_req_bits_data),
    .io_daxi2apb_req_bits_mask(dpath_io_daxi2apb_req_bits_mask),
    .io_daxi2apb_resp_valid(dpath_io_daxi2apb_resp_valid),
    .io_daxi2apb_resp_bits_data(dpath_io_daxi2apb_resp_bits_data),
    .io_uart_resp_valid(dpath_io_uart_resp_valid),
    .io_ctrl_inst(dpath_io_ctrl_inst),
    .io_ctrl_pc_sel(dpath_io_ctrl_pc_sel),
    .io_ctrl_inst_kill(dpath_io_ctrl_inst_kill),
    .io_ctrl_A_sel(dpath_io_ctrl_A_sel),
    .io_ctrl_B_sel(dpath_io_ctrl_B_sel),
    .io_ctrl_imm_sel(dpath_io_ctrl_imm_sel),
    .io_ctrl_alu_op(dpath_io_ctrl_alu_op),
    .io_ctrl_br_type(dpath_io_ctrl_br_type),
    .io_ctrl_st_type(dpath_io_ctrl_st_type),
    .io_ctrl_ld_type(dpath_io_ctrl_ld_type),
    .io_ctrl_wb_sel(dpath_io_ctrl_wb_sel),
    .io_ctrl_wb_en(dpath_io_ctrl_wb_en),
    .io_ctrl_csr_cmd(dpath_io_ctrl_csr_cmd),
    .io_ctrl_illegal(dpath_io_ctrl_illegal)
  );
  Control ctrl ( // @[Core.scala 31:20]
    .io_inst(ctrl_io_inst),
    .io_pc_sel(ctrl_io_pc_sel),
    .io_inst_kill(ctrl_io_inst_kill),
    .io_A_sel(ctrl_io_A_sel),
    .io_B_sel(ctrl_io_B_sel),
    .io_imm_sel(ctrl_io_imm_sel),
    .io_alu_op(ctrl_io_alu_op),
    .io_br_type(ctrl_io_br_type),
    .io_st_type(ctrl_io_st_type),
    .io_ld_type(ctrl_io_ld_type),
    .io_wb_sel(ctrl_io_wb_sel),
    .io_wb_en(ctrl_io_wb_en),
    .io_csr_cmd(ctrl_io_csr_cmd),
    .io_illegal(ctrl_io_illegal)
  );
  assign io_host_tohost = dpath_io_host_tohost; // @[Core.scala 33:11]
  assign io_dcache_abort = dpath_io_dcache_abort; // @[Core.scala 35:19]
  assign io_iaxi2apb_req_valid = dpath_io_iaxi2apb_req_valid; // @[Core.scala 36:21]
  assign io_iaxi2apb_req_bits_addr = dpath_io_iaxi2apb_req_bits_addr; // @[Core.scala 36:21]
  assign io_daxi2apb_req_valid = dpath_io_daxi2apb_req_valid; // @[Core.scala 37:21]
  assign io_daxi2apb_req_bits_addr = dpath_io_daxi2apb_req_bits_addr; // @[Core.scala 37:21]
  assign io_daxi2apb_req_bits_data = dpath_io_daxi2apb_req_bits_data; // @[Core.scala 37:21]
  assign io_daxi2apb_req_bits_mask = dpath_io_daxi2apb_req_bits_mask; // @[Core.scala 37:21]
  assign dpath_clock = clock;
  assign dpath_reset = reset;
  assign dpath_io_host_fromhost_valid = io_host_fromhost_valid; // @[Core.scala 33:11]
  assign dpath_io_host_fromhost_bits = io_host_fromhost_bits; // @[Core.scala 33:11]
  assign dpath_io_icache_resp_valid = io_icache_resp_valid; // @[Core.scala 34:19]
  assign dpath_io_icache_resp_bits_data = io_icache_resp_bits_data; // @[Core.scala 34:19]
  assign dpath_io_dcache_resp_valid = io_dcache_resp_valid; // @[Core.scala 35:19]
  assign dpath_io_dcache_resp_bits_data = io_dcache_resp_bits_data; // @[Core.scala 35:19]
  assign dpath_io_iaxi2apb_resp_valid = io_iaxi2apb_resp_valid; // @[Core.scala 36:21]
  assign dpath_io_iaxi2apb_resp_bits_data = io_iaxi2apb_resp_bits_data; // @[Core.scala 36:21]
  assign dpath_io_daxi2apb_resp_valid = io_daxi2apb_resp_valid; // @[Core.scala 37:21]
  assign dpath_io_daxi2apb_resp_bits_data = io_daxi2apb_resp_bits_data; // @[Core.scala 37:21]
  assign dpath_io_uart_resp_valid = io_uart_resp_valid; // @[Core.scala 38:17]
  assign dpath_io_ctrl_pc_sel = ctrl_io_pc_sel; // @[Core.scala 39:17]
  assign dpath_io_ctrl_inst_kill = ctrl_io_inst_kill; // @[Core.scala 39:17]
  assign dpath_io_ctrl_A_sel = ctrl_io_A_sel; // @[Core.scala 39:17]
  assign dpath_io_ctrl_B_sel = ctrl_io_B_sel; // @[Core.scala 39:17]
  assign dpath_io_ctrl_imm_sel = ctrl_io_imm_sel; // @[Core.scala 39:17]
  assign dpath_io_ctrl_alu_op = ctrl_io_alu_op; // @[Core.scala 39:17]
  assign dpath_io_ctrl_br_type = ctrl_io_br_type; // @[Core.scala 39:17]
  assign dpath_io_ctrl_st_type = ctrl_io_st_type; // @[Core.scala 39:17]
  assign dpath_io_ctrl_ld_type = ctrl_io_ld_type; // @[Core.scala 39:17]
  assign dpath_io_ctrl_wb_sel = ctrl_io_wb_sel; // @[Core.scala 39:17]
  assign dpath_io_ctrl_wb_en = ctrl_io_wb_en; // @[Core.scala 39:17]
  assign dpath_io_ctrl_csr_cmd = ctrl_io_csr_cmd; // @[Core.scala 39:17]
  assign dpath_io_ctrl_illegal = ctrl_io_illegal; // @[Core.scala 39:17]
  assign ctrl_io_inst = dpath_io_ctrl_inst; // @[Core.scala 39:17]
endmodule
module Cache(
  input         clock,
  input         reset,
  input         io_cpu_abort,
  output        io_cpu_resp_valid,
  output [31:0] io_cpu_resp_bits_data,
  input         io_nasti_aw_ready,
  output        io_nasti_aw_valid,
  output [31:0] io_nasti_aw_bits_addr,
  input         io_nasti_w_ready,
  output        io_nasti_w_valid,
  output [63:0] io_nasti_w_bits_data,
  output        io_nasti_w_bits_last,
  output        io_nasti_b_ready,
  input         io_nasti_b_valid,
  input         io_nasti_ar_ready,
  output        io_nasti_ar_valid,
  output        io_nasti_r_ready,
  input         io_nasti_r_valid,
  input  [63:0] io_nasti_r_bits_data
);
`ifdef RANDOMIZE_MEM_INIT
  reg [31:0] _RAND_0;
`endif // RANDOMIZE_MEM_INIT
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_1;
  reg [31:0] _RAND_2;
  reg [31:0] _RAND_3;
  reg [255:0] _RAND_4;
  reg [255:0] _RAND_5;
  reg [31:0] _RAND_6;
  reg [31:0] _RAND_7;
  reg [31:0] _RAND_8;
  reg [63:0] _RAND_9;
  reg [63:0] _RAND_10;
`endif // RANDOMIZE_REG_INIT
  reg [19:0] metaMem_tag [0:255]; // @[Cache.scala 62:28]
  wire  metaMem_tag_rmeta_en; // @[Cache.scala 62:28]
  wire [7:0] metaMem_tag_rmeta_addr; // @[Cache.scala 62:28]
  wire [19:0] metaMem_tag_rmeta_data; // @[Cache.scala 62:28]
  wire [19:0] metaMem_tag_MPORT_data; // @[Cache.scala 62:28]
  wire [7:0] metaMem_tag_MPORT_addr; // @[Cache.scala 62:28]
  wire  metaMem_tag_MPORT_mask; // @[Cache.scala 62:28]
  wire  metaMem_tag_MPORT_en; // @[Cache.scala 62:28]
  reg  metaMem_tag_rmeta_en_pipe_0;
  reg [7:0] metaMem_tag_rmeta_addr_pipe_0;
  reg [2:0] state; // @[Cache.scala 58:22]
  reg [255:0] v; // @[Cache.scala 60:18]
  reg [255:0] d; // @[Cache.scala 61:18]
  wire  _T = io_nasti_r_ready & io_nasti_r_valid; // @[Decoupled.scala 50:35]
  reg  read_count; // @[Counter.scala 62:40]
  wire  read_wrap_out = _T & read_count; // @[Counter.scala 120:{16,23}]
  wire  _T_1 = io_nasti_w_ready & io_nasti_w_valid; // @[Decoupled.scala 50:35]
  reg  write_count; // @[Counter.scala 62:40]
  wire  write_wrap_out = _T_1 & write_count; // @[Counter.scala 120:{16,23}]
  wire  is_idle = state == 3'h0; // @[Cache.scala 74:23]
  wire  is_read = state == 3'h1; // @[Cache.scala 75:23]
  wire  is_write = state == 3'h2; // @[Cache.scala 76:24]
  wire  is_alloc = state == 3'h6 & read_wrap_out; // @[Cache.scala 77:36]
  reg  is_alloc_reg; // @[Cache.scala 78:29]
  wire  hit = v[0] & metaMem_tag_rmeta_data == 20'h0; // @[Cache.scala 98:21]
  wire  _wen_T = hit | is_alloc_reg; // @[Cache.scala 81:30]
  wire  wen = is_write & (hit | is_alloc_reg) & ~io_cpu_abort | is_alloc; // @[Cache.scala 81:64]
  reg [63:0] refill_buf_0; // @[Cache.scala 95:23]
  reg [63:0] refill_buf_1; // @[Cache.scala 95:23]
  wire [127:0] _read_T = {refill_buf_1,refill_buf_0}; // @[Cache.scala 96:43]
  wire [127:0] read = is_alloc_reg ? _read_T : 128'h0; // @[Cache.scala 96:17]
  wire  _wmask_T = ~is_alloc; // @[Cache.scala 113:19]
  wire [255:0] _v_T_1 = v | 256'h1; // @[Cache.scala 121:18]
  wire [255:0] _d_T_2 = d | 256'h1; // @[Cache.scala 122:18]
  wire [255:0] _d_T_3 = ~d; // @[Cache.scala 122:18]
  wire [255:0] _d_T_4 = _d_T_3 | 256'h1; // @[Cache.scala 122:18]
  wire [255:0] _d_T_5 = ~_d_T_4; // @[Cache.scala 122:18]
  wire [27:0] _io_nasti_aw_bits_T = {metaMem_tag_rmeta_data,8'h0}; // @[Cat.scala 31:58]
  wire [31:0] _GEN_146 = {_io_nasti_aw_bits_T, 4'h0}; // @[Cache.scala 151:30]
  wire [34:0] _io_nasti_aw_bits_T_1 = {{3'd0}, _GEN_146}; // @[Cache.scala 151:30]
  wire  is_dirty = v[0] & d[0]; // @[Cache.scala 168:29]
  wire  _io_nasti_ar_valid_T = ~is_dirty; // @[Cache.scala 184:30]
  wire  _T_29 = io_nasti_aw_ready & io_nasti_aw_valid; // @[Decoupled.scala 50:35]
  wire  _T_30 = io_nasti_ar_ready & io_nasti_ar_valid; // @[Decoupled.scala 50:35]
  wire [2:0] _GEN_107 = _T_30 ? 3'h6 : state; // @[Cache.scala 187:38 188:17 58:22]
  wire [2:0] _GEN_108 = _T_29 ? 3'h3 : _GEN_107; // @[Cache.scala 185:32 186:17]
  wire  _GEN_110 = hit ? 1'h0 : is_dirty; // @[Cache.scala 176:17 156:21 183:27]
  wire  _GEN_111 = hit ? 1'h0 : ~is_dirty; // @[Cache.scala 176:17 141:21 184:27]
  wire [2:0] _GEN_114 = _wen_T | io_cpu_abort ? 3'h0 : _GEN_108; // @[Cache.scala 193:49 194:15]
  wire  _GEN_115 = _wen_T | io_cpu_abort ? 1'h0 : is_dirty; // @[Cache.scala 156:21 193:49 196:27]
  wire  _GEN_116 = _wen_T | io_cpu_abort ? 1'h0 : _io_nasti_ar_valid_T; // @[Cache.scala 141:21 193:49 197:27]
  wire [2:0] _GEN_117 = write_wrap_out ? 3'h4 : state; // @[Cache.scala 207:28 208:15 58:22]
  wire  _T_44 = io_nasti_b_ready & io_nasti_b_valid; // @[Decoupled.scala 50:35]
  wire [2:0] _GEN_118 = _T_44 ? 3'h5 : state; // @[Cache.scala 213:29 214:15 58:22]
  wire [2:0] _GEN_120 = read_wrap_out ? 3'h0 : state; // @[Cache.scala 224:27 225:15 58:22]
  wire [2:0] _GEN_121 = 3'h6 == state ? _GEN_120 : state; // @[Cache.scala 169:17 58:22]
  wire [2:0] _GEN_123 = 3'h5 == state ? _GEN_107 : _GEN_121; // @[Cache.scala 169:17]
  wire [2:0] _GEN_125 = 3'h4 == state ? _GEN_118 : _GEN_123; // @[Cache.scala 169:17]
  wire  _GEN_126 = 3'h4 == state ? 1'h0 : 3'h5 == state; // @[Cache.scala 169:17 141:21]
  wire [2:0] _GEN_128 = 3'h3 == state ? _GEN_117 : _GEN_125; // @[Cache.scala 169:17]
  wire  _GEN_129 = 3'h3 == state ? 1'h0 : 3'h4 == state; // @[Cache.scala 169:17 165:20]
  wire  _GEN_130 = 3'h3 == state ? 1'h0 : _GEN_126; // @[Cache.scala 169:17 141:21]
  wire  _GEN_132 = 3'h2 == state & _GEN_115; // @[Cache.scala 169:17 156:21]
  wire  _GEN_133 = 3'h2 == state ? _GEN_116 : _GEN_130; // @[Cache.scala 169:17]
  wire  _GEN_134 = 3'h2 == state ? 1'h0 : 3'h3 == state; // @[Cache.scala 169:17 163:20]
  wire  _GEN_135 = 3'h2 == state ? 1'h0 : _GEN_129; // @[Cache.scala 169:17 165:20]
  wire  _GEN_137 = 3'h1 == state ? _GEN_110 : _GEN_132; // @[Cache.scala 169:17]
  wire  _GEN_138 = 3'h1 == state ? _GEN_111 : _GEN_133; // @[Cache.scala 169:17]
  wire  _GEN_139 = 3'h1 == state ? 1'h0 : _GEN_134; // @[Cache.scala 169:17 163:20]
  wire  _GEN_140 = 3'h1 == state ? 1'h0 : _GEN_135; // @[Cache.scala 169:17 165:20]
  assign metaMem_tag_rmeta_en = metaMem_tag_rmeta_en_pipe_0;
  assign metaMem_tag_rmeta_addr = metaMem_tag_rmeta_addr_pipe_0;
  assign metaMem_tag_rmeta_data = metaMem_tag[metaMem_tag_rmeta_addr]; // @[Cache.scala 62:28]
  assign metaMem_tag_MPORT_data = 20'h0;
  assign metaMem_tag_MPORT_addr = 8'h0;
  assign metaMem_tag_MPORT_mask = 1'h1;
  assign metaMem_tag_MPORT_en = wen & is_alloc;
  assign io_cpu_resp_valid = is_idle | is_read & hit | is_alloc_reg; // @[Cache.scala 102:50]
  assign io_cpu_resp_bits_data = read[31:0]; // @[Cache.scala 101:62]
  assign io_nasti_aw_valid = 3'h0 == state ? 1'h0 : _GEN_137; // @[Cache.scala 169:17 156:21]
  assign io_nasti_aw_bits_addr = _io_nasti_aw_bits_T_1[31:0]; // @[nasti.scala 63:18 65:13]
  assign io_nasti_w_valid = 3'h0 == state ? 1'h0 : _GEN_139; // @[Cache.scala 169:17 163:20]
  assign io_nasti_w_bits_data = write_count ? read[127:64] : read[63:0]; // @[nasti.scala 95:{12,12}]
  assign io_nasti_w_bits_last = _T_1 & write_count; // @[Counter.scala 120:{16,23}]
  assign io_nasti_b_ready = 3'h0 == state ? 1'h0 : _GEN_140; // @[Cache.scala 169:17 165:20]
  assign io_nasti_ar_valid = 3'h0 == state ? 1'h0 : _GEN_138; // @[Cache.scala 169:17 141:21]
  assign io_nasti_r_ready = state == 3'h6; // @[Cache.scala 143:29]
  always @(posedge clock) begin
    if (metaMem_tag_MPORT_en & metaMem_tag_MPORT_mask) begin
      metaMem_tag[metaMem_tag_MPORT_addr] <= metaMem_tag_MPORT_data; // @[Cache.scala 62:28]
    end
    metaMem_tag_rmeta_en_pipe_0 <= 1'h0;
    if (1'h0) begin
      metaMem_tag_rmeta_addr_pipe_0 <= 8'h0;
    end
    if (reset) begin // @[Cache.scala 58:22]
      state <= 3'h0; // @[Cache.scala 58:22]
    end else if (!(3'h0 == state)) begin // @[Cache.scala 169:17]
      if (3'h1 == state) begin // @[Cache.scala 169:17]
        if (hit) begin // @[Cache.scala 176:17]
          state <= 3'h0;
        end else begin
          state <= _GEN_108;
        end
      end else if (3'h2 == state) begin // @[Cache.scala 169:17]
        state <= _GEN_114;
      end else begin
        state <= _GEN_128;
      end
    end
    if (reset) begin // @[Cache.scala 60:18]
      v <= 256'h0; // @[Cache.scala 60:18]
    end else if (wen) begin // @[Cache.scala 120:13]
      v <= _v_T_1; // @[Cache.scala 121:7]
    end
    if (reset) begin // @[Cache.scala 61:18]
      d <= 256'h0; // @[Cache.scala 61:18]
    end else if (wen) begin // @[Cache.scala 120:13]
      if (_wmask_T) begin // @[Cache.scala 122:18]
        d <= _d_T_2;
      end else begin
        d <= _d_T_5;
      end
    end
    if (reset) begin // @[Counter.scala 62:40]
      read_count <= 1'h0; // @[Counter.scala 62:40]
    end else if (_T) begin // @[Counter.scala 120:16]
      read_count <= read_count + 1'h1; // @[Counter.scala 78:15]
    end
    if (reset) begin // @[Counter.scala 62:40]
      write_count <= 1'h0; // @[Counter.scala 62:40]
    end else if (_T_1) begin // @[Counter.scala 120:16]
      write_count <= write_count + 1'h1; // @[Counter.scala 78:15]
    end
    is_alloc_reg <= state == 3'h6 & read_wrap_out; // @[Cache.scala 77:36]
    if (_T) begin // @[Cache.scala 144:25]
      if (~read_count) begin // @[Cache.scala 145:28]
        refill_buf_0 <= io_nasti_r_bits_data; // @[Cache.scala 145:28]
      end
    end
    if (_T) begin // @[Cache.scala 144:25]
      if (read_count) begin // @[Cache.scala 145:28]
        refill_buf_1 <= io_nasti_r_bits_data; // @[Cache.scala 145:28]
      end
    end
  end
// Register and memory initialization
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
`ifdef FIRRTL_BEFORE_INITIAL
`FIRRTL_BEFORE_INITIAL
`endif
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
`ifdef RANDOMIZE_MEM_INIT
  _RAND_0 = {1{`RANDOM}};
  for (initvar = 0; initvar < 256; initvar = initvar+1)
    metaMem_tag[initvar] = _RAND_0[19:0];
`endif // RANDOMIZE_MEM_INIT
`ifdef RANDOMIZE_REG_INIT
  _RAND_1 = {1{`RANDOM}};
  metaMem_tag_rmeta_en_pipe_0 = _RAND_1[0:0];
  _RAND_2 = {1{`RANDOM}};
  metaMem_tag_rmeta_addr_pipe_0 = _RAND_2[7:0];
  _RAND_3 = {1{`RANDOM}};
  state = _RAND_3[2:0];
  _RAND_4 = {8{`RANDOM}};
  v = _RAND_4[255:0];
  _RAND_5 = {8{`RANDOM}};
  d = _RAND_5[255:0];
  _RAND_6 = {1{`RANDOM}};
  read_count = _RAND_6[0:0];
  _RAND_7 = {1{`RANDOM}};
  write_count = _RAND_7[0:0];
  _RAND_8 = {1{`RANDOM}};
  is_alloc_reg = _RAND_8[0:0];
  _RAND_9 = {2{`RANDOM}};
  refill_buf_0 = _RAND_9[63:0];
  _RAND_10 = {2{`RANDOM}};
  refill_buf_1 = _RAND_10[63:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
module Mini2axi(
  input         clock,
  input         reset,
  input         io_cpu_req_valid,
  input  [31:0] io_cpu_req_bits_addr,
  input  [31:0] io_cpu_req_bits_data,
  input  [3:0]  io_cpu_req_bits_mask,
  output        io_cpu_resp_valid,
  output [31:0] io_cpu_resp_bits_data,
  input         io_nasti_aw_ready,
  output        io_nasti_aw_valid,
  output [31:0] io_nasti_aw_bits_addr,
  input         io_nasti_w_ready,
  output        io_nasti_w_valid,
  output [63:0] io_nasti_w_bits_data,
  output [7:0]  io_nasti_w_bits_strb,
  output        io_nasti_b_ready,
  input         io_nasti_b_valid,
  input         io_nasti_ar_ready,
  output        io_nasti_ar_valid,
  output [31:0] io_nasti_ar_bits_addr,
  output        io_nasti_r_ready,
  input         io_nasti_r_valid,
  input  [63:0] io_nasti_r_bits_data
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
  reg [31:0] _RAND_1;
  reg [31:0] _RAND_2;
  reg [31:0] _RAND_3;
`endif // RANDOMIZE_REG_INIT
  reg [2:0] state; // @[mini2axi.scala 25:24]
  wire  is_idle = state == 3'h0; // @[mini2axi.scala 27:25]
  reg [3:0] reg_mask; // @[mini2axi.scala 34:27]
  reg [31:0] reg_addr; // @[mini2axi.scala 35:27]
  reg [31:0] reg_data; // @[mini2axi.scala 36:27]
  wire  _io_cpu_resp_valid_T = io_nasti_r_ready & io_nasti_r_valid; // @[Decoupled.scala 50:35]
  wire  _io_cpu_resp_valid_T_2 = io_nasti_b_ready & io_nasti_b_valid; // @[Decoupled.scala 50:35]
  wire [7:0] _GEN_52 = {reg_mask, 4'h0}; // @[mini2axi.scala 73:64]
  wire [10:0] _io_nasti_w_bits_x3_T_2 = {{3'd0}, _GEN_52}; // @[mini2axi.scala 73:64]
  wire [10:0] io_nasti_w_bits_x3 = io_nasti_aw_bits_addr[2] ? _io_nasti_w_bits_x3_T_2 : {{7'd0}, reg_mask}; // @[mini2axi.scala 73:17]
  wire [1:0] _state_T_1 = |io_cpu_req_bits_mask ? 2'h3 : 2'h1; // @[mini2axi.scala 86:29]
  wire [31:0] _GEN_0 = io_cpu_req_valid ? io_cpu_req_bits_addr : reg_addr; // @[mini2axi.scala 82:36 83:26 35:27]
  wire [31:0] _GEN_1 = io_cpu_req_valid ? io_cpu_req_bits_data : reg_data; // @[mini2axi.scala 82:36 84:26 36:27]
  wire [3:0] _GEN_2 = io_cpu_req_valid ? io_cpu_req_bits_mask : reg_mask; // @[mini2axi.scala 82:36 85:26 34:27]
  wire  _T_6 = io_nasti_ar_ready & io_nasti_ar_valid; // @[Decoupled.scala 50:35]
  wire [1:0] _GEN_5 = io_cpu_req_valid ? _state_T_1 : 2'h0; // @[mini2axi.scala 101:27 103:27 97:40]
  wire [31:0] _GEN_6 = _io_cpu_resp_valid_T ? _GEN_1 : reg_data; // @[mini2axi.scala 36:27 96:35]
  wire [3:0] _GEN_7 = _io_cpu_resp_valid_T ? _GEN_2 : reg_mask; // @[mini2axi.scala 34:27 96:35]
  wire [31:0] _GEN_8 = _io_cpu_resp_valid_T ? _GEN_0 : reg_addr; // @[mini2axi.scala 35:27 96:35]
  wire [2:0] _GEN_9 = _io_cpu_resp_valid_T ? {{1'd0}, _GEN_5} : state; // @[mini2axi.scala 25:24 96:35]
  wire  _T_14 = io_nasti_aw_ready & io_nasti_aw_valid; // @[Decoupled.scala 50:35]
  wire [2:0] _GEN_10 = _T_14 ? 3'h4 : state; // @[mini2axi.scala 110:36 111:23 25:24]
  wire  _T_18 = io_nasti_w_ready & io_nasti_w_valid; // @[Decoupled.scala 50:35]
  wire [2:0] _GEN_11 = _T_18 ? 3'h5 : state; // @[mini2axi.scala 117:35 118:23 25:24]
  wire [31:0] _GEN_13 = _io_cpu_resp_valid_T_2 ? _GEN_1 : reg_data; // @[mini2axi.scala 123:35 36:27]
  wire [3:0] _GEN_14 = _io_cpu_resp_valid_T_2 ? _GEN_2 : reg_mask; // @[mini2axi.scala 123:35 34:27]
  wire [31:0] _GEN_15 = _io_cpu_resp_valid_T_2 ? _GEN_0 : reg_addr; // @[mini2axi.scala 123:35 35:27]
  wire [2:0] _GEN_16 = _io_cpu_resp_valid_T_2 ? {{1'd0}, _GEN_5} : state; // @[mini2axi.scala 123:35 25:24]
  wire [31:0] _GEN_17 = 3'h5 == state ? _GEN_13 : reg_data; // @[mini2axi.scala 80:19 36:27]
  wire [3:0] _GEN_18 = 3'h5 == state ? _GEN_14 : reg_mask; // @[mini2axi.scala 80:19 34:27]
  wire [31:0] _GEN_19 = 3'h5 == state ? _GEN_15 : reg_addr; // @[mini2axi.scala 80:19 35:27]
  wire [2:0] _GEN_20 = 3'h5 == state ? _GEN_16 : state; // @[mini2axi.scala 80:19 25:24]
  wire [2:0] _GEN_22 = 3'h4 == state ? _GEN_11 : _GEN_20; // @[mini2axi.scala 80:19]
  wire [31:0] _GEN_23 = 3'h4 == state ? reg_data : _GEN_17; // @[mini2axi.scala 80:19 36:27]
  wire [3:0] _GEN_24 = 3'h4 == state ? reg_mask : _GEN_18; // @[mini2axi.scala 80:19 34:27]
  wire [31:0] _GEN_25 = 3'h4 == state ? reg_addr : _GEN_19; // @[mini2axi.scala 80:19 35:27]
  wire [2:0] _GEN_27 = 3'h3 == state ? _GEN_10 : _GEN_22; // @[mini2axi.scala 80:19]
  wire  _GEN_28 = 3'h3 == state ? 1'h0 : 3'h4 == state; // @[mini2axi.scala 80:19 76:22]
  wire [31:0] _GEN_29 = 3'h3 == state ? reg_data : _GEN_23; // @[mini2axi.scala 80:19 36:27]
  wire [3:0] _GEN_30 = 3'h3 == state ? reg_mask : _GEN_24; // @[mini2axi.scala 80:19 34:27]
  wire [31:0] _GEN_31 = 3'h3 == state ? reg_addr : _GEN_25; // @[mini2axi.scala 80:19 35:27]
  wire  _GEN_36 = 3'h2 == state ? 1'h0 : 3'h3 == state; // @[mini2axi.scala 80:19 65:23]
  wire  _GEN_37 = 3'h2 == state ? 1'h0 : _GEN_28; // @[mini2axi.scala 80:19 76:22]
  wire  _GEN_43 = 3'h1 == state ? 1'h0 : _GEN_36; // @[mini2axi.scala 80:19 65:23]
  wire  _GEN_44 = 3'h1 == state ? 1'h0 : _GEN_37; // @[mini2axi.scala 80:19 76:22]
  assign io_cpu_resp_valid = is_idle | _io_cpu_resp_valid_T | _io_cpu_resp_valid_T_2; // @[mini2axi.scala 42:53]
  assign io_cpu_resp_bits_data = io_nasti_r_bits_data[31:0]; // @[mini2axi.scala 39:27]
  assign io_nasti_aw_valid = 3'h0 == state ? 1'h0 : _GEN_43; // @[mini2axi.scala 80:19 65:23]
  assign io_nasti_aw_bits_addr = reg_addr; // @[nasti.scala 63:18 65:13]
  assign io_nasti_w_valid = 3'h0 == state ? 1'h0 : _GEN_44; // @[mini2axi.scala 80:19 76:22]
  assign io_nasti_w_bits_data = {reg_data,reg_data}; // @[mini2axi.scala 72:18]
  assign io_nasti_w_bits_strb = io_nasti_w_bits_x3[7:0]; // @[nasti.scala 92:17 94:12]
  assign io_nasti_b_ready = state == 3'h5; // @[mini2axi.scala 32:31]
  assign io_nasti_ar_valid = 3'h0 == state ? 1'h0 : 3'h1 == state; // @[mini2axi.scala 80:19 53:23]
  assign io_nasti_ar_bits_addr = io_cpu_req_bits_addr; // @[nasti.scala 63:18 65:13]
  assign io_nasti_r_ready = state == 3'h2; // @[mini2axi.scala 29:30]
  always @(posedge clock) begin
    if (reset) begin // @[mini2axi.scala 25:24]
      state <= 3'h0; // @[mini2axi.scala 25:24]
    end else if (3'h0 == state) begin // @[mini2axi.scala 80:19]
      if (io_cpu_req_valid) begin // @[mini2axi.scala 82:36]
        state <= {{1'd0}, _state_T_1}; // @[mini2axi.scala 86:23]
      end
    end else if (3'h1 == state) begin // @[mini2axi.scala 80:19]
      if (_T_6) begin // @[mini2axi.scala 91:36]
        state <= 3'h2; // @[mini2axi.scala 92:23]
      end
    end else if (3'h2 == state) begin // @[mini2axi.scala 80:19]
      state <= _GEN_9;
    end else begin
      state <= _GEN_27;
    end
    if (reset) begin // @[mini2axi.scala 34:27]
      reg_mask <= 4'h0; // @[mini2axi.scala 34:27]
    end else if (3'h0 == state) begin // @[mini2axi.scala 80:19]
      if (io_cpu_req_valid) begin // @[mini2axi.scala 82:36]
        reg_mask <= io_cpu_req_bits_mask; // @[mini2axi.scala 85:26]
      end
    end else if (!(3'h1 == state)) begin // @[mini2axi.scala 80:19]
      if (3'h2 == state) begin // @[mini2axi.scala 80:19]
        reg_mask <= _GEN_7;
      end else begin
        reg_mask <= _GEN_30;
      end
    end
    if (reset) begin // @[mini2axi.scala 35:27]
      reg_addr <= 32'h0; // @[mini2axi.scala 35:27]
    end else if (3'h0 == state) begin // @[mini2axi.scala 80:19]
      if (io_cpu_req_valid) begin // @[mini2axi.scala 82:36]
        reg_addr <= io_cpu_req_bits_addr; // @[mini2axi.scala 83:26]
      end
    end else if (!(3'h1 == state)) begin // @[mini2axi.scala 80:19]
      if (3'h2 == state) begin // @[mini2axi.scala 80:19]
        reg_addr <= _GEN_8;
      end else begin
        reg_addr <= _GEN_31;
      end
    end
    if (reset) begin // @[mini2axi.scala 36:27]
      reg_data <= 32'h0; // @[mini2axi.scala 36:27]
    end else if (3'h0 == state) begin // @[mini2axi.scala 80:19]
      if (io_cpu_req_valid) begin // @[mini2axi.scala 82:36]
        reg_data <= io_cpu_req_bits_data; // @[mini2axi.scala 84:26]
      end
    end else if (!(3'h1 == state)) begin // @[mini2axi.scala 80:19]
      if (3'h2 == state) begin // @[mini2axi.scala 80:19]
        reg_data <= _GEN_6;
      end else begin
        reg_data <= _GEN_29;
      end
    end
  end
// Register and memory initialization
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
`ifdef FIRRTL_BEFORE_INITIAL
`FIRRTL_BEFORE_INITIAL
`endif
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
`ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  state = _RAND_0[2:0];
  _RAND_1 = {1{`RANDOM}};
  reg_mask = _RAND_1[3:0];
  _RAND_2 = {1{`RANDOM}};
  reg_addr = _RAND_2[31:0];
  _RAND_3 = {1{`RANDOM}};
  reg_data = _RAND_3[31:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
module MemArbiter(
  input         clock,
  input         reset,
  output        io_icache_ar_ready,
  input         io_icache_ar_valid,
  input         io_icache_r_ready,
  output        io_icache_r_valid,
  output [63:0] io_icache_r_bits_data,
  output        io_dcache_aw_ready,
  input         io_dcache_aw_valid,
  input  [31:0] io_dcache_aw_bits_addr,
  output        io_dcache_w_ready,
  input         io_dcache_w_valid,
  input  [63:0] io_dcache_w_bits_data,
  input         io_dcache_w_bits_last,
  input         io_dcache_b_ready,
  output        io_dcache_b_valid,
  output        io_dcache_ar_ready,
  input         io_dcache_ar_valid,
  input         io_dcache_r_ready,
  output        io_dcache_r_valid,
  output [63:0] io_dcache_r_bits_data,
  output        io_iaxi2apb_ar_ready,
  input         io_iaxi2apb_ar_valid,
  input  [31:0] io_iaxi2apb_ar_bits_addr,
  input         io_iaxi2apb_r_ready,
  output        io_iaxi2apb_r_valid,
  output [63:0] io_iaxi2apb_r_bits_data,
  output        io_daxi2apb_aw_ready,
  input         io_daxi2apb_aw_valid,
  input  [31:0] io_daxi2apb_aw_bits_addr,
  output        io_daxi2apb_w_ready,
  input         io_daxi2apb_w_valid,
  input  [63:0] io_daxi2apb_w_bits_data,
  input  [7:0]  io_daxi2apb_w_bits_strb,
  input         io_daxi2apb_b_ready,
  output        io_daxi2apb_b_valid,
  output        io_daxi2apb_ar_ready,
  input         io_daxi2apb_ar_valid,
  input  [31:0] io_daxi2apb_ar_bits_addr,
  input         io_daxi2apb_r_ready,
  output        io_daxi2apb_r_valid,
  output [63:0] io_daxi2apb_r_bits_data,
  output        io_uart_aw_ready,
  input         io_uart_aw_valid,
  input  [31:0] io_uart_aw_bits_addr,
  output        io_uart_w_ready,
  input         io_uart_w_valid,
  input  [63:0] io_uart_w_bits_data,
  input  [7:0]  io_uart_w_bits_strb,
  input         io_uart_b_ready,
  output        io_uart_b_valid,
  output        io_uart_ar_ready,
  input         io_uart_ar_valid,
  input  [31:0] io_uart_ar_bits_addr,
  input         io_uart_r_ready,
  output        io_uart_r_valid,
  output [63:0] io_uart_r_bits_data,
  input         io_nasti_aw_ready,
  output        io_nasti_aw_valid,
  output [31:0] io_nasti_aw_bits_addr,
  output [7:0]  io_nasti_aw_bits_len,
  output [2:0]  io_nasti_aw_bits_size,
  input         io_nasti_w_ready,
  output        io_nasti_w_valid,
  output [63:0] io_nasti_w_bits_data,
  output [7:0]  io_nasti_w_bits_strb,
  output        io_nasti_w_bits_last,
  output        io_nasti_b_ready,
  input         io_nasti_b_valid,
  input         io_nasti_ar_ready,
  output        io_nasti_ar_valid,
  output [31:0] io_nasti_ar_bits_addr,
  output [7:0]  io_nasti_ar_bits_len,
  output [2:0]  io_nasti_ar_bits_size,
  output        io_nasti_r_ready,
  input         io_nasti_r_valid,
  input  [63:0] io_nasti_r_bits_data,
  input         io_nasti_r_bits_last
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
`endif // RANDOMIZE_REG_INIT
  reg [3:0] state; // @[Tile.scala 29:22]
  wire [31:0] _io_nasti_aw_bits_T_2 = io_dcache_aw_valid ? io_dcache_aw_bits_addr : io_uart_aw_bits_addr; // @[Tile.scala 35:59]
  wire [2:0] _io_nasti_aw_bits_T_4 = io_dcache_aw_valid ? 3'h3 : 3'h2; // @[Tile.scala 36:59]
  wire [7:0] _io_nasti_aw_bits_T_6 = io_dcache_aw_valid ? 8'h1 : 8'h0; // @[Tile.scala 37:59]
  wire  _io_nasti_aw_valid_T_2 = state == 4'h0; // @[Tile.scala 40:98]
  wire [63:0] _io_nasti_w_bits_T = io_dcache_w_valid ? io_dcache_w_bits_data : io_uart_w_bits_data; // @[Tile.scala 51:63]
  wire [7:0] _io_nasti_w_bits_T_2 = io_dcache_w_valid ? 8'hff : io_uart_w_bits_strb; // @[Tile.scala 52:64]
  wire  _io_nasti_w_bits_T_4 = io_dcache_w_valid ? io_dcache_w_bits_last : 1'h1; // @[Tile.scala 53:63]
  wire  _io_nasti_w_valid_T = state == 4'h3; // @[Tile.scala 56:53]
  wire  _io_nasti_w_valid_T_2 = state == 4'h7; // @[Tile.scala 56:100]
  wire  _io_nasti_w_valid_T_5 = state == 4'ha; // @[Tile.scala 56:147]
  wire  _io_daxi2apb_b_valid_T = state == 4'h4; // @[Tile.scala 67:52]
  wire  _io_dcache_b_valid_T = state == 4'h8; // @[Tile.scala 68:50]
  wire  _io_uart_b_valid_T = state == 4'hb; // @[Tile.scala 69:48]
  wire [31:0] _io_nasti_ar_bits_T_4 = io_icache_ar_valid ? 32'h0 : io_uart_ar_bits_addr; // @[Tile.scala 77:158]
  wire [31:0] _io_nasti_ar_bits_T_5 = io_dcache_ar_valid ? 32'h0 : _io_nasti_ar_bits_T_4; // @[Tile.scala 77:111]
  wire [31:0] _io_nasti_ar_bits_T_6 = io_iaxi2apb_ar_valid ? io_iaxi2apb_ar_bits_addr : _io_nasti_ar_bits_T_5; // @[Tile.scala 77:59]
  wire [2:0] _io_nasti_ar_bits_T_8 = io_icache_ar_valid ? 3'h3 : 3'h2; // @[Tile.scala 78:158]
  wire [2:0] _io_nasti_ar_bits_T_9 = io_dcache_ar_valid ? 3'h3 : _io_nasti_ar_bits_T_8; // @[Tile.scala 78:111]
  wire [2:0] _io_nasti_ar_bits_T_10 = io_iaxi2apb_ar_valid ? 3'h2 : _io_nasti_ar_bits_T_9; // @[Tile.scala 78:59]
  wire [7:0] _io_nasti_ar_bits_T_12 = io_icache_ar_valid ? 8'h1 : 8'h0; // @[Tile.scala 79:158]
  wire [7:0] _io_nasti_ar_bits_T_13 = io_dcache_ar_valid ? 8'h1 : _io_nasti_ar_bits_T_12; // @[Tile.scala 79:111]
  wire [7:0] _io_nasti_ar_bits_T_14 = io_iaxi2apb_ar_valid ? 8'h0 : _io_nasti_ar_bits_T_13; // @[Tile.scala 79:59]
  wire  _io_nasti_ar_valid_T_4 = ~io_nasti_aw_valid; // @[Tile.scala 82:5]
  wire  _io_nasti_ar_valid_T_5 = (io_iaxi2apb_ar_valid | io_daxi2apb_ar_valid | io_icache_ar_valid | io_dcache_ar_valid
     | io_uart_ar_valid) & _io_nasti_ar_valid_T_4; // @[Tile.scala 81:135]
  wire  _io_iaxi2apb_r_bits_id_T = state == 4'h1; // @[Tile.scala 97:42]
  wire  _io_daxi2apb_r_bits_id_T = state == 4'h2; // @[Tile.scala 102:42]
  wire  _io_icache_r_bits_id_T = state == 4'h5; // @[Tile.scala 108:40]
  wire  _io_dcache_r_bits_id_T = state == 4'h6; // @[Tile.scala 113:40]
  wire  _io_uart_r_bits_id_T = state == 4'h9; // @[Tile.scala 118:38]
  wire  _io_nasti_r_ready_T_3 = io_dcache_r_ready & _io_dcache_r_bits_id_T; // @[Tile.scala 129:23]
  wire  _io_nasti_r_ready_T_4 = io_icache_r_ready & _io_icache_r_bits_id_T | _io_nasti_r_ready_T_3; // @[Tile.scala 128:66]
  wire  _io_nasti_r_ready_T_6 = io_uart_r_ready & _io_uart_r_bits_id_T; // @[Tile.scala 130:21]
  wire  _io_nasti_r_ready_T_7 = _io_nasti_r_ready_T_4 | _io_nasti_r_ready_T_6; // @[Tile.scala 129:48]
  wire  _io_nasti_r_ready_T_9 = io_iaxi2apb_r_ready & _io_iaxi2apb_r_bits_id_T; // @[Tile.scala 131:25]
  wire  _io_nasti_r_ready_T_10 = _io_nasti_r_ready_T_7 | _io_nasti_r_ready_T_9; // @[Tile.scala 130:44]
  wire  _io_nasti_r_ready_T_12 = io_daxi2apb_r_ready & _io_daxi2apb_r_bits_id_T; // @[Tile.scala 132:25]
  wire  _T_3 = io_daxi2apb_aw_ready & io_daxi2apb_aw_valid; // @[Decoupled.scala 50:35]
  wire  _T_4 = io_daxi2apb_ar_ready & io_daxi2apb_ar_valid; // @[Decoupled.scala 50:35]
  wire  _T_5 = io_uart_aw_ready & io_uart_aw_valid; // @[Decoupled.scala 50:35]
  wire  _T_6 = io_uart_ar_ready & io_uart_ar_valid; // @[Decoupled.scala 50:35]
  wire  _T_7 = io_iaxi2apb_ar_ready & io_iaxi2apb_ar_valid; // @[Decoupled.scala 50:35]
  wire  _T_8 = io_dcache_aw_ready & io_dcache_aw_valid; // @[Decoupled.scala 50:35]
  wire  _T_9 = io_dcache_ar_ready & io_dcache_ar_valid; // @[Decoupled.scala 50:35]
  wire  _T_10 = io_icache_ar_ready & io_icache_ar_valid; // @[Decoupled.scala 50:35]
  wire [3:0] _GEN_0 = _T_10 ? 4'h5 : state; // @[Tile.scala 151:37 152:15 29:22]
  wire [3:0] _GEN_1 = _T_9 ? 4'h6 : _GEN_0; // @[Tile.scala 149:37 150:15]
  wire [3:0] _GEN_2 = _T_8 ? 4'h7 : _GEN_1; // @[Tile.scala 147:36 148:15]
  wire [3:0] _GEN_3 = _T_7 ? 4'h1 : _GEN_2; // @[Tile.scala 144:39 145:15]
  wire [3:0] _GEN_4 = _T_6 ? 4'h9 : _GEN_3; // @[Tile.scala 142:35 143:15]
  wire [3:0] _GEN_5 = _T_5 ? 4'ha : _GEN_4; // @[Tile.scala 140:35 141:15]
  wire  _T_14 = io_nasti_r_ready & io_nasti_r_valid; // @[Decoupled.scala 50:35]
  wire [3:0] _GEN_8 = _T_14 & io_nasti_r_bits_last ? 4'h0 : state; // @[Tile.scala 157:53 158:15 29:22]
  wire  _T_24 = io_daxi2apb_w_ready & io_daxi2apb_w_valid; // @[Decoupled.scala 50:35]
  wire [3:0] _GEN_10 = _T_24 ? 4'h4 : state; // @[Tile.scala 167:59 168:15 29:22]
  wire  _T_29 = io_nasti_b_ready & io_nasti_b_valid; // @[Decoupled.scala 50:35]
  wire [3:0] _GEN_11 = _T_29 ? 4'h0 : state; // @[Tile.scala 172:29 173:15 29:22]
  wire  _T_43 = io_dcache_w_ready & io_dcache_w_valid; // @[Decoupled.scala 50:35]
  wire [3:0] _GEN_14 = _T_43 & io_dcache_w_bits_last ? 4'h8 : state; // @[Tile.scala 188:55 189:15 29:22]
  wire  _T_57 = io_uart_w_ready & io_uart_w_valid; // @[Decoupled.scala 50:35]
  wire [3:0] _GEN_17 = _T_57 ? 4'hb : state; // @[Tile.scala 204:51 205:15 29:22]
  wire [3:0] _GEN_19 = 4'hb == state ? _GEN_11 : state; // @[Tile.scala 134:17 29:22]
  wire [3:0] _GEN_20 = 4'ha == state ? _GEN_17 : _GEN_19; // @[Tile.scala 134:17]
  wire [3:0] _GEN_21 = 4'h9 == state ? _GEN_8 : _GEN_20; // @[Tile.scala 134:17]
  wire [3:0] _GEN_22 = 4'h8 == state ? _GEN_11 : _GEN_21; // @[Tile.scala 134:17]
  wire [3:0] _GEN_23 = 4'h7 == state ? _GEN_14 : _GEN_22; // @[Tile.scala 134:17]
  wire [3:0] _GEN_24 = 4'h6 == state ? _GEN_8 : _GEN_23; // @[Tile.scala 134:17]
  wire [3:0] _GEN_25 = 4'h5 == state ? _GEN_8 : _GEN_24; // @[Tile.scala 134:17]
  wire [3:0] _GEN_26 = 4'h4 == state ? _GEN_11 : _GEN_25; // @[Tile.scala 134:17]
  wire [3:0] _GEN_27 = 4'h3 == state ? _GEN_10 : _GEN_26; // @[Tile.scala 134:17]
  assign io_icache_ar_ready = io_dcache_ar_ready & ~io_dcache_ar_valid; // @[Tile.scala 91:44]
  assign io_icache_r_valid = io_nasti_r_valid & _io_icache_r_bits_id_T; // @[Tile.scala 125:41]
  assign io_icache_r_bits_data = _io_icache_r_bits_id_T ? io_nasti_r_bits_data : 64'h0; // @[Tile.scala 109:33]
  assign io_dcache_aw_ready = io_nasti_aw_ready & _io_nasti_aw_valid_T_2; // @[Tile.scala 42:43]
  assign io_dcache_w_ready = io_nasti_w_ready & _io_nasti_w_valid_T_2; // @[Tile.scala 58:41]
  assign io_dcache_b_valid = io_nasti_b_valid & state == 4'h8; // @[Tile.scala 68:41]
  assign io_dcache_ar_ready = io_iaxi2apb_ar_ready & ~io_iaxi2apb_ar_valid; // @[Tile.scala 90:46]
  assign io_dcache_r_valid = io_nasti_r_valid & _io_dcache_r_bits_id_T; // @[Tile.scala 126:41]
  assign io_dcache_r_bits_data = _io_dcache_r_bits_id_T ? io_nasti_r_bits_data : 64'h0; // @[Tile.scala 114:33]
  assign io_iaxi2apb_ar_ready = io_uart_ar_ready & ~io_uart_ar_valid; // @[Tile.scala 86:44]
  assign io_iaxi2apb_r_valid = io_nasti_r_valid & _io_iaxi2apb_r_bits_id_T; // @[Tile.scala 123:43]
  assign io_iaxi2apb_r_bits_data = _io_iaxi2apb_r_bits_id_T ? io_nasti_r_bits_data : 64'h0; // @[Tile.scala 98:35]
  assign io_daxi2apb_aw_ready = io_nasti_aw_ready & _io_nasti_aw_valid_T_2; // @[Tile.scala 41:45]
  assign io_daxi2apb_w_ready = io_nasti_w_ready & _io_nasti_w_valid_T; // @[Tile.scala 57:43]
  assign io_daxi2apb_b_valid = io_nasti_b_valid & state == 4'h4; // @[Tile.scala 67:43]
  assign io_daxi2apb_ar_ready = io_nasti_ar_ready & _io_nasti_ar_valid_T_4 & _io_nasti_aw_valid_T_2; // @[Tile.scala 84:67]
  assign io_daxi2apb_r_valid = io_nasti_r_valid & _io_daxi2apb_r_bits_id_T; // @[Tile.scala 124:43]
  assign io_daxi2apb_r_bits_data = _io_daxi2apb_r_bits_id_T ? io_nasti_r_bits_data : 64'h0; // @[Tile.scala 103:35]
  assign io_uart_aw_ready = io_nasti_aw_ready & _io_nasti_aw_valid_T_2; // @[Tile.scala 43:41]
  assign io_uart_w_ready = io_nasti_w_ready & _io_nasti_w_valid_T_5; // @[Tile.scala 59:39]
  assign io_uart_b_valid = io_nasti_b_valid & state == 4'hb; // @[Tile.scala 69:39]
  assign io_uart_ar_ready = io_daxi2apb_ar_ready & ~io_daxi2apb_ar_valid; // @[Tile.scala 85:44]
  assign io_uart_r_valid = io_nasti_r_valid & _io_uart_r_bits_id_T; // @[Tile.scala 127:39]
  assign io_uart_r_bits_data = _io_uart_r_bits_id_T ? io_nasti_r_bits_data : 64'h0; // @[Tile.scala 119:31]
  assign io_nasti_aw_valid = (io_daxi2apb_aw_valid | io_dcache_aw_valid | io_uart_aw_valid) & state == 4'h0; // @[Tile.scala 40:89]
  assign io_nasti_aw_bits_addr = io_daxi2apb_aw_valid ? io_daxi2apb_aw_bits_addr : _io_nasti_aw_bits_T_2; // @[Tile.scala 35:7]
  assign io_nasti_aw_bits_len = io_daxi2apb_aw_valid ? 8'h0 : _io_nasti_aw_bits_T_6; // @[Tile.scala 37:7]
  assign io_nasti_aw_bits_size = io_daxi2apb_aw_valid ? 3'h2 : _io_nasti_aw_bits_T_4; // @[Tile.scala 36:7]
  assign io_nasti_w_valid = io_daxi2apb_w_valid & state == 4'h3 | io_dcache_w_valid & state == 4'h7 | io_uart_w_valid &
    state == 4'ha; // @[Tile.scala 56:118]
  assign io_nasti_w_bits_data = io_daxi2apb_w_valid ? io_daxi2apb_w_bits_data : _io_nasti_w_bits_T; // @[Tile.scala 51:13]
  assign io_nasti_w_bits_strb = io_daxi2apb_w_valid ? io_daxi2apb_w_bits_strb : _io_nasti_w_bits_T_2; // @[Tile.scala 52:13]
  assign io_nasti_w_bits_last = io_daxi2apb_w_valid | _io_nasti_w_bits_T_4; // @[Tile.scala 53:13]
  assign io_nasti_b_ready = io_daxi2apb_b_ready & _io_daxi2apb_b_valid_T | io_dcache_b_ready & _io_dcache_b_valid_T |
    io_uart_b_ready & _io_uart_b_valid_T; // @[Tile.scala 70:114]
  assign io_nasti_ar_valid = _io_nasti_ar_valid_T_5 & _io_nasti_aw_valid_T_2; // @[Tile.scala 82:24]
  assign io_nasti_ar_bits_addr = io_daxi2apb_ar_valid ? io_daxi2apb_ar_bits_addr : _io_nasti_ar_bits_T_6; // @[Tile.scala 77:7]
  assign io_nasti_ar_bits_len = io_daxi2apb_ar_valid ? 8'h0 : _io_nasti_ar_bits_T_14; // @[Tile.scala 79:7]
  assign io_nasti_ar_bits_size = io_daxi2apb_ar_valid ? 3'h2 : _io_nasti_ar_bits_T_10; // @[Tile.scala 78:7]
  assign io_nasti_r_ready = _io_nasti_r_ready_T_10 | _io_nasti_r_ready_T_12; // @[Tile.scala 131:48]
  always @(posedge clock) begin
    if (reset) begin // @[Tile.scala 29:22]
      state <= 4'h0; // @[Tile.scala 29:22]
    end else if (4'h0 == state) begin // @[Tile.scala 134:17]
      if (_T_3) begin // @[Tile.scala 136:33]
        state <= 4'h3; // @[Tile.scala 137:15]
      end else if (_T_4) begin // @[Tile.scala 138:39]
        state <= 4'h2; // @[Tile.scala 139:15]
      end else begin
        state <= _GEN_5;
      end
    end else if (4'h1 == state) begin // @[Tile.scala 134:17]
      state <= _GEN_8;
    end else if (4'h2 == state) begin // @[Tile.scala 134:17]
      state <= _GEN_8;
    end else begin
      state <= _GEN_27;
    end
  end
// Register and memory initialization
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
`ifdef FIRRTL_BEFORE_INITIAL
`FIRRTL_BEFORE_INITIAL
`endif
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
`ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  state = _RAND_0[3:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
module Tile(
  input         clock,
  input         reset,
  input         io_host_fromhost_valid,
  input  [31:0] io_host_fromhost_bits,
  output [31:0] io_host_tohost,
  input         io_nasti_aw_ready,
  output        io_nasti_aw_valid,
  output [3:0]  io_nasti_aw_bits_id,
  output [31:0] io_nasti_aw_bits_addr,
  output [7:0]  io_nasti_aw_bits_len,
  output [2:0]  io_nasti_aw_bits_size,
  output [1:0]  io_nasti_aw_bits_burst,
  output        io_nasti_aw_bits_lock,
  output [3:0]  io_nasti_aw_bits_cache,
  output [2:0]  io_nasti_aw_bits_prot,
  output [3:0]  io_nasti_aw_bits_qos,
  input         io_nasti_w_ready,
  output        io_nasti_w_valid,
  output [63:0] io_nasti_w_bits_data,
  output [7:0]  io_nasti_w_bits_strb,
  output        io_nasti_w_bits_last,
  output        io_nasti_b_ready,
  input         io_nasti_b_valid,
  input  [3:0]  io_nasti_b_bits_id,
  input  [1:0]  io_nasti_b_bits_resp,
  input         io_nasti_ar_ready,
  output        io_nasti_ar_valid,
  output [3:0]  io_nasti_ar_bits_id,
  output [31:0] io_nasti_ar_bits_addr,
  output [7:0]  io_nasti_ar_bits_len,
  output [2:0]  io_nasti_ar_bits_size,
  output [1:0]  io_nasti_ar_bits_burst,
  output        io_nasti_ar_bits_lock,
  output [3:0]  io_nasti_ar_bits_cache,
  output [2:0]  io_nasti_ar_bits_prot,
  output [3:0]  io_nasti_ar_bits_qos,
  output        io_nasti_r_ready,
  input         io_nasti_r_valid,
  input  [3:0]  io_nasti_r_bits_id,
  input  [63:0] io_nasti_r_bits_data,
  input  [1:0]  io_nasti_r_bits_resp,
  input         io_nasti_r_bits_last
);
  wire  core_clock; // @[Tile.scala 228:20]
  wire  core_reset; // @[Tile.scala 228:20]
  wire  core_io_host_fromhost_valid; // @[Tile.scala 228:20]
  wire [31:0] core_io_host_fromhost_bits; // @[Tile.scala 228:20]
  wire [31:0] core_io_host_tohost; // @[Tile.scala 228:20]
  wire  core_io_icache_resp_valid; // @[Tile.scala 228:20]
  wire [31:0] core_io_icache_resp_bits_data; // @[Tile.scala 228:20]
  wire  core_io_dcache_abort; // @[Tile.scala 228:20]
  wire  core_io_dcache_resp_valid; // @[Tile.scala 228:20]
  wire [31:0] core_io_dcache_resp_bits_data; // @[Tile.scala 228:20]
  wire  core_io_iaxi2apb_req_valid; // @[Tile.scala 228:20]
  wire [31:0] core_io_iaxi2apb_req_bits_addr; // @[Tile.scala 228:20]
  wire  core_io_iaxi2apb_resp_valid; // @[Tile.scala 228:20]
  wire [31:0] core_io_iaxi2apb_resp_bits_data; // @[Tile.scala 228:20]
  wire  core_io_daxi2apb_req_valid; // @[Tile.scala 228:20]
  wire [31:0] core_io_daxi2apb_req_bits_addr; // @[Tile.scala 228:20]
  wire [31:0] core_io_daxi2apb_req_bits_data; // @[Tile.scala 228:20]
  wire [3:0] core_io_daxi2apb_req_bits_mask; // @[Tile.scala 228:20]
  wire  core_io_daxi2apb_resp_valid; // @[Tile.scala 228:20]
  wire [31:0] core_io_daxi2apb_resp_bits_data; // @[Tile.scala 228:20]
  wire  core_io_uart_resp_valid; // @[Tile.scala 228:20]
  wire  icache_clock; // @[Tile.scala 229:22]
  wire  icache_reset; // @[Tile.scala 229:22]
  wire  icache_io_cpu_abort; // @[Tile.scala 229:22]
  wire  icache_io_cpu_resp_valid; // @[Tile.scala 229:22]
  wire [31:0] icache_io_cpu_resp_bits_data; // @[Tile.scala 229:22]
  wire  icache_io_nasti_aw_ready; // @[Tile.scala 229:22]
  wire  icache_io_nasti_aw_valid; // @[Tile.scala 229:22]
  wire [31:0] icache_io_nasti_aw_bits_addr; // @[Tile.scala 229:22]
  wire  icache_io_nasti_w_ready; // @[Tile.scala 229:22]
  wire  icache_io_nasti_w_valid; // @[Tile.scala 229:22]
  wire [63:0] icache_io_nasti_w_bits_data; // @[Tile.scala 229:22]
  wire  icache_io_nasti_w_bits_last; // @[Tile.scala 229:22]
  wire  icache_io_nasti_b_ready; // @[Tile.scala 229:22]
  wire  icache_io_nasti_b_valid; // @[Tile.scala 229:22]
  wire  icache_io_nasti_ar_ready; // @[Tile.scala 229:22]
  wire  icache_io_nasti_ar_valid; // @[Tile.scala 229:22]
  wire  icache_io_nasti_r_ready; // @[Tile.scala 229:22]
  wire  icache_io_nasti_r_valid; // @[Tile.scala 229:22]
  wire [63:0] icache_io_nasti_r_bits_data; // @[Tile.scala 229:22]
  wire  dcache_clock; // @[Tile.scala 230:22]
  wire  dcache_reset; // @[Tile.scala 230:22]
  wire  dcache_io_cpu_abort; // @[Tile.scala 230:22]
  wire  dcache_io_cpu_resp_valid; // @[Tile.scala 230:22]
  wire [31:0] dcache_io_cpu_resp_bits_data; // @[Tile.scala 230:22]
  wire  dcache_io_nasti_aw_ready; // @[Tile.scala 230:22]
  wire  dcache_io_nasti_aw_valid; // @[Tile.scala 230:22]
  wire [31:0] dcache_io_nasti_aw_bits_addr; // @[Tile.scala 230:22]
  wire  dcache_io_nasti_w_ready; // @[Tile.scala 230:22]
  wire  dcache_io_nasti_w_valid; // @[Tile.scala 230:22]
  wire [63:0] dcache_io_nasti_w_bits_data; // @[Tile.scala 230:22]
  wire  dcache_io_nasti_w_bits_last; // @[Tile.scala 230:22]
  wire  dcache_io_nasti_b_ready; // @[Tile.scala 230:22]
  wire  dcache_io_nasti_b_valid; // @[Tile.scala 230:22]
  wire  dcache_io_nasti_ar_ready; // @[Tile.scala 230:22]
  wire  dcache_io_nasti_ar_valid; // @[Tile.scala 230:22]
  wire  dcache_io_nasti_r_ready; // @[Tile.scala 230:22]
  wire  dcache_io_nasti_r_valid; // @[Tile.scala 230:22]
  wire [63:0] dcache_io_nasti_r_bits_data; // @[Tile.scala 230:22]
  wire  iaxi2apb_clock; // @[Tile.scala 231:24]
  wire  iaxi2apb_reset; // @[Tile.scala 231:24]
  wire  iaxi2apb_io_cpu_req_valid; // @[Tile.scala 231:24]
  wire [31:0] iaxi2apb_io_cpu_req_bits_addr; // @[Tile.scala 231:24]
  wire [31:0] iaxi2apb_io_cpu_req_bits_data; // @[Tile.scala 231:24]
  wire [3:0] iaxi2apb_io_cpu_req_bits_mask; // @[Tile.scala 231:24]
  wire  iaxi2apb_io_cpu_resp_valid; // @[Tile.scala 231:24]
  wire [31:0] iaxi2apb_io_cpu_resp_bits_data; // @[Tile.scala 231:24]
  wire  iaxi2apb_io_nasti_aw_ready; // @[Tile.scala 231:24]
  wire  iaxi2apb_io_nasti_aw_valid; // @[Tile.scala 231:24]
  wire [31:0] iaxi2apb_io_nasti_aw_bits_addr; // @[Tile.scala 231:24]
  wire  iaxi2apb_io_nasti_w_ready; // @[Tile.scala 231:24]
  wire  iaxi2apb_io_nasti_w_valid; // @[Tile.scala 231:24]
  wire [63:0] iaxi2apb_io_nasti_w_bits_data; // @[Tile.scala 231:24]
  wire [7:0] iaxi2apb_io_nasti_w_bits_strb; // @[Tile.scala 231:24]
  wire  iaxi2apb_io_nasti_b_ready; // @[Tile.scala 231:24]
  wire  iaxi2apb_io_nasti_b_valid; // @[Tile.scala 231:24]
  wire  iaxi2apb_io_nasti_ar_ready; // @[Tile.scala 231:24]
  wire  iaxi2apb_io_nasti_ar_valid; // @[Tile.scala 231:24]
  wire [31:0] iaxi2apb_io_nasti_ar_bits_addr; // @[Tile.scala 231:24]
  wire  iaxi2apb_io_nasti_r_ready; // @[Tile.scala 231:24]
  wire  iaxi2apb_io_nasti_r_valid; // @[Tile.scala 231:24]
  wire [63:0] iaxi2apb_io_nasti_r_bits_data; // @[Tile.scala 231:24]
  wire  daxi2apb_clock; // @[Tile.scala 232:24]
  wire  daxi2apb_reset; // @[Tile.scala 232:24]
  wire  daxi2apb_io_cpu_req_valid; // @[Tile.scala 232:24]
  wire [31:0] daxi2apb_io_cpu_req_bits_addr; // @[Tile.scala 232:24]
  wire [31:0] daxi2apb_io_cpu_req_bits_data; // @[Tile.scala 232:24]
  wire [3:0] daxi2apb_io_cpu_req_bits_mask; // @[Tile.scala 232:24]
  wire  daxi2apb_io_cpu_resp_valid; // @[Tile.scala 232:24]
  wire [31:0] daxi2apb_io_cpu_resp_bits_data; // @[Tile.scala 232:24]
  wire  daxi2apb_io_nasti_aw_ready; // @[Tile.scala 232:24]
  wire  daxi2apb_io_nasti_aw_valid; // @[Tile.scala 232:24]
  wire [31:0] daxi2apb_io_nasti_aw_bits_addr; // @[Tile.scala 232:24]
  wire  daxi2apb_io_nasti_w_ready; // @[Tile.scala 232:24]
  wire  daxi2apb_io_nasti_w_valid; // @[Tile.scala 232:24]
  wire [63:0] daxi2apb_io_nasti_w_bits_data; // @[Tile.scala 232:24]
  wire [7:0] daxi2apb_io_nasti_w_bits_strb; // @[Tile.scala 232:24]
  wire  daxi2apb_io_nasti_b_ready; // @[Tile.scala 232:24]
  wire  daxi2apb_io_nasti_b_valid; // @[Tile.scala 232:24]
  wire  daxi2apb_io_nasti_ar_ready; // @[Tile.scala 232:24]
  wire  daxi2apb_io_nasti_ar_valid; // @[Tile.scala 232:24]
  wire [31:0] daxi2apb_io_nasti_ar_bits_addr; // @[Tile.scala 232:24]
  wire  daxi2apb_io_nasti_r_ready; // @[Tile.scala 232:24]
  wire  daxi2apb_io_nasti_r_valid; // @[Tile.scala 232:24]
  wire [63:0] daxi2apb_io_nasti_r_bits_data; // @[Tile.scala 232:24]
  wire  uart_clock; // @[Tile.scala 233:20]
  wire  uart_reset; // @[Tile.scala 233:20]
  wire  uart_io_cpu_req_valid; // @[Tile.scala 233:20]
  wire [31:0] uart_io_cpu_req_bits_addr; // @[Tile.scala 233:20]
  wire [31:0] uart_io_cpu_req_bits_data; // @[Tile.scala 233:20]
  wire [3:0] uart_io_cpu_req_bits_mask; // @[Tile.scala 233:20]
  wire  uart_io_cpu_resp_valid; // @[Tile.scala 233:20]
  wire [31:0] uart_io_cpu_resp_bits_data; // @[Tile.scala 233:20]
  wire  uart_io_nasti_aw_ready; // @[Tile.scala 233:20]
  wire  uart_io_nasti_aw_valid; // @[Tile.scala 233:20]
  wire [31:0] uart_io_nasti_aw_bits_addr; // @[Tile.scala 233:20]
  wire  uart_io_nasti_w_ready; // @[Tile.scala 233:20]
  wire  uart_io_nasti_w_valid; // @[Tile.scala 233:20]
  wire [63:0] uart_io_nasti_w_bits_data; // @[Tile.scala 233:20]
  wire [7:0] uart_io_nasti_w_bits_strb; // @[Tile.scala 233:20]
  wire  uart_io_nasti_b_ready; // @[Tile.scala 233:20]
  wire  uart_io_nasti_b_valid; // @[Tile.scala 233:20]
  wire  uart_io_nasti_ar_ready; // @[Tile.scala 233:20]
  wire  uart_io_nasti_ar_valid; // @[Tile.scala 233:20]
  wire [31:0] uart_io_nasti_ar_bits_addr; // @[Tile.scala 233:20]
  wire  uart_io_nasti_r_ready; // @[Tile.scala 233:20]
  wire  uart_io_nasti_r_valid; // @[Tile.scala 233:20]
  wire [63:0] uart_io_nasti_r_bits_data; // @[Tile.scala 233:20]
  wire  arb_clock; // @[Tile.scala 234:19]
  wire  arb_reset; // @[Tile.scala 234:19]
  wire  arb_io_icache_ar_ready; // @[Tile.scala 234:19]
  wire  arb_io_icache_ar_valid; // @[Tile.scala 234:19]
  wire  arb_io_icache_r_ready; // @[Tile.scala 234:19]
  wire  arb_io_icache_r_valid; // @[Tile.scala 234:19]
  wire [63:0] arb_io_icache_r_bits_data; // @[Tile.scala 234:19]
  wire  arb_io_dcache_aw_ready; // @[Tile.scala 234:19]
  wire  arb_io_dcache_aw_valid; // @[Tile.scala 234:19]
  wire [31:0] arb_io_dcache_aw_bits_addr; // @[Tile.scala 234:19]
  wire  arb_io_dcache_w_ready; // @[Tile.scala 234:19]
  wire  arb_io_dcache_w_valid; // @[Tile.scala 234:19]
  wire [63:0] arb_io_dcache_w_bits_data; // @[Tile.scala 234:19]
  wire  arb_io_dcache_w_bits_last; // @[Tile.scala 234:19]
  wire  arb_io_dcache_b_ready; // @[Tile.scala 234:19]
  wire  arb_io_dcache_b_valid; // @[Tile.scala 234:19]
  wire  arb_io_dcache_ar_ready; // @[Tile.scala 234:19]
  wire  arb_io_dcache_ar_valid; // @[Tile.scala 234:19]
  wire  arb_io_dcache_r_ready; // @[Tile.scala 234:19]
  wire  arb_io_dcache_r_valid; // @[Tile.scala 234:19]
  wire [63:0] arb_io_dcache_r_bits_data; // @[Tile.scala 234:19]
  wire  arb_io_iaxi2apb_ar_ready; // @[Tile.scala 234:19]
  wire  arb_io_iaxi2apb_ar_valid; // @[Tile.scala 234:19]
  wire [31:0] arb_io_iaxi2apb_ar_bits_addr; // @[Tile.scala 234:19]
  wire  arb_io_iaxi2apb_r_ready; // @[Tile.scala 234:19]
  wire  arb_io_iaxi2apb_r_valid; // @[Tile.scala 234:19]
  wire [63:0] arb_io_iaxi2apb_r_bits_data; // @[Tile.scala 234:19]
  wire  arb_io_daxi2apb_aw_ready; // @[Tile.scala 234:19]
  wire  arb_io_daxi2apb_aw_valid; // @[Tile.scala 234:19]
  wire [31:0] arb_io_daxi2apb_aw_bits_addr; // @[Tile.scala 234:19]
  wire  arb_io_daxi2apb_w_ready; // @[Tile.scala 234:19]
  wire  arb_io_daxi2apb_w_valid; // @[Tile.scala 234:19]
  wire [63:0] arb_io_daxi2apb_w_bits_data; // @[Tile.scala 234:19]
  wire [7:0] arb_io_daxi2apb_w_bits_strb; // @[Tile.scala 234:19]
  wire  arb_io_daxi2apb_b_ready; // @[Tile.scala 234:19]
  wire  arb_io_daxi2apb_b_valid; // @[Tile.scala 234:19]
  wire  arb_io_daxi2apb_ar_ready; // @[Tile.scala 234:19]
  wire  arb_io_daxi2apb_ar_valid; // @[Tile.scala 234:19]
  wire [31:0] arb_io_daxi2apb_ar_bits_addr; // @[Tile.scala 234:19]
  wire  arb_io_daxi2apb_r_ready; // @[Tile.scala 234:19]
  wire  arb_io_daxi2apb_r_valid; // @[Tile.scala 234:19]
  wire [63:0] arb_io_daxi2apb_r_bits_data; // @[Tile.scala 234:19]
  wire  arb_io_uart_aw_ready; // @[Tile.scala 234:19]
  wire  arb_io_uart_aw_valid; // @[Tile.scala 234:19]
  wire [31:0] arb_io_uart_aw_bits_addr; // @[Tile.scala 234:19]
  wire  arb_io_uart_w_ready; // @[Tile.scala 234:19]
  wire  arb_io_uart_w_valid; // @[Tile.scala 234:19]
  wire [63:0] arb_io_uart_w_bits_data; // @[Tile.scala 234:19]
  wire [7:0] arb_io_uart_w_bits_strb; // @[Tile.scala 234:19]
  wire  arb_io_uart_b_ready; // @[Tile.scala 234:19]
  wire  arb_io_uart_b_valid; // @[Tile.scala 234:19]
  wire  arb_io_uart_ar_ready; // @[Tile.scala 234:19]
  wire  arb_io_uart_ar_valid; // @[Tile.scala 234:19]
  wire [31:0] arb_io_uart_ar_bits_addr; // @[Tile.scala 234:19]
  wire  arb_io_uart_r_ready; // @[Tile.scala 234:19]
  wire  arb_io_uart_r_valid; // @[Tile.scala 234:19]
  wire [63:0] arb_io_uart_r_bits_data; // @[Tile.scala 234:19]
  wire  arb_io_nasti_aw_ready; // @[Tile.scala 234:19]
  wire  arb_io_nasti_aw_valid; // @[Tile.scala 234:19]
  wire [31:0] arb_io_nasti_aw_bits_addr; // @[Tile.scala 234:19]
  wire [7:0] arb_io_nasti_aw_bits_len; // @[Tile.scala 234:19]
  wire [2:0] arb_io_nasti_aw_bits_size; // @[Tile.scala 234:19]
  wire  arb_io_nasti_w_ready; // @[Tile.scala 234:19]
  wire  arb_io_nasti_w_valid; // @[Tile.scala 234:19]
  wire [63:0] arb_io_nasti_w_bits_data; // @[Tile.scala 234:19]
  wire [7:0] arb_io_nasti_w_bits_strb; // @[Tile.scala 234:19]
  wire  arb_io_nasti_w_bits_last; // @[Tile.scala 234:19]
  wire  arb_io_nasti_b_ready; // @[Tile.scala 234:19]
  wire  arb_io_nasti_b_valid; // @[Tile.scala 234:19]
  wire  arb_io_nasti_ar_ready; // @[Tile.scala 234:19]
  wire  arb_io_nasti_ar_valid; // @[Tile.scala 234:19]
  wire [31:0] arb_io_nasti_ar_bits_addr; // @[Tile.scala 234:19]
  wire [7:0] arb_io_nasti_ar_bits_len; // @[Tile.scala 234:19]
  wire [2:0] arb_io_nasti_ar_bits_size; // @[Tile.scala 234:19]
  wire  arb_io_nasti_r_ready; // @[Tile.scala 234:19]
  wire  arb_io_nasti_r_valid; // @[Tile.scala 234:19]
  wire [63:0] arb_io_nasti_r_bits_data; // @[Tile.scala 234:19]
  wire  arb_io_nasti_r_bits_last; // @[Tile.scala 234:19]
  Core core ( // @[Tile.scala 228:20]
    .clock(core_clock),
    .reset(core_reset),
    .io_host_fromhost_valid(core_io_host_fromhost_valid),
    .io_host_fromhost_bits(core_io_host_fromhost_bits),
    .io_host_tohost(core_io_host_tohost),
    .io_icache_resp_valid(core_io_icache_resp_valid),
    .io_icache_resp_bits_data(core_io_icache_resp_bits_data),
    .io_dcache_abort(core_io_dcache_abort),
    .io_dcache_resp_valid(core_io_dcache_resp_valid),
    .io_dcache_resp_bits_data(core_io_dcache_resp_bits_data),
    .io_iaxi2apb_req_valid(core_io_iaxi2apb_req_valid),
    .io_iaxi2apb_req_bits_addr(core_io_iaxi2apb_req_bits_addr),
    .io_iaxi2apb_resp_valid(core_io_iaxi2apb_resp_valid),
    .io_iaxi2apb_resp_bits_data(core_io_iaxi2apb_resp_bits_data),
    .io_daxi2apb_req_valid(core_io_daxi2apb_req_valid),
    .io_daxi2apb_req_bits_addr(core_io_daxi2apb_req_bits_addr),
    .io_daxi2apb_req_bits_data(core_io_daxi2apb_req_bits_data),
    .io_daxi2apb_req_bits_mask(core_io_daxi2apb_req_bits_mask),
    .io_daxi2apb_resp_valid(core_io_daxi2apb_resp_valid),
    .io_daxi2apb_resp_bits_data(core_io_daxi2apb_resp_bits_data),
    .io_uart_resp_valid(core_io_uart_resp_valid)
  );
  Cache icache ( // @[Tile.scala 229:22]
    .clock(icache_clock),
    .reset(icache_reset),
    .io_cpu_abort(icache_io_cpu_abort),
    .io_cpu_resp_valid(icache_io_cpu_resp_valid),
    .io_cpu_resp_bits_data(icache_io_cpu_resp_bits_data),
    .io_nasti_aw_ready(icache_io_nasti_aw_ready),
    .io_nasti_aw_valid(icache_io_nasti_aw_valid),
    .io_nasti_aw_bits_addr(icache_io_nasti_aw_bits_addr),
    .io_nasti_w_ready(icache_io_nasti_w_ready),
    .io_nasti_w_valid(icache_io_nasti_w_valid),
    .io_nasti_w_bits_data(icache_io_nasti_w_bits_data),
    .io_nasti_w_bits_last(icache_io_nasti_w_bits_last),
    .io_nasti_b_ready(icache_io_nasti_b_ready),
    .io_nasti_b_valid(icache_io_nasti_b_valid),
    .io_nasti_ar_ready(icache_io_nasti_ar_ready),
    .io_nasti_ar_valid(icache_io_nasti_ar_valid),
    .io_nasti_r_ready(icache_io_nasti_r_ready),
    .io_nasti_r_valid(icache_io_nasti_r_valid),
    .io_nasti_r_bits_data(icache_io_nasti_r_bits_data)
  );
  Cache dcache ( // @[Tile.scala 230:22]
    .clock(dcache_clock),
    .reset(dcache_reset),
    .io_cpu_abort(dcache_io_cpu_abort),
    .io_cpu_resp_valid(dcache_io_cpu_resp_valid),
    .io_cpu_resp_bits_data(dcache_io_cpu_resp_bits_data),
    .io_nasti_aw_ready(dcache_io_nasti_aw_ready),
    .io_nasti_aw_valid(dcache_io_nasti_aw_valid),
    .io_nasti_aw_bits_addr(dcache_io_nasti_aw_bits_addr),
    .io_nasti_w_ready(dcache_io_nasti_w_ready),
    .io_nasti_w_valid(dcache_io_nasti_w_valid),
    .io_nasti_w_bits_data(dcache_io_nasti_w_bits_data),
    .io_nasti_w_bits_last(dcache_io_nasti_w_bits_last),
    .io_nasti_b_ready(dcache_io_nasti_b_ready),
    .io_nasti_b_valid(dcache_io_nasti_b_valid),
    .io_nasti_ar_ready(dcache_io_nasti_ar_ready),
    .io_nasti_ar_valid(dcache_io_nasti_ar_valid),
    .io_nasti_r_ready(dcache_io_nasti_r_ready),
    .io_nasti_r_valid(dcache_io_nasti_r_valid),
    .io_nasti_r_bits_data(dcache_io_nasti_r_bits_data)
  );
  Mini2axi iaxi2apb ( // @[Tile.scala 231:24]
    .clock(iaxi2apb_clock),
    .reset(iaxi2apb_reset),
    .io_cpu_req_valid(iaxi2apb_io_cpu_req_valid),
    .io_cpu_req_bits_addr(iaxi2apb_io_cpu_req_bits_addr),
    .io_cpu_req_bits_data(iaxi2apb_io_cpu_req_bits_data),
    .io_cpu_req_bits_mask(iaxi2apb_io_cpu_req_bits_mask),
    .io_cpu_resp_valid(iaxi2apb_io_cpu_resp_valid),
    .io_cpu_resp_bits_data(iaxi2apb_io_cpu_resp_bits_data),
    .io_nasti_aw_ready(iaxi2apb_io_nasti_aw_ready),
    .io_nasti_aw_valid(iaxi2apb_io_nasti_aw_valid),
    .io_nasti_aw_bits_addr(iaxi2apb_io_nasti_aw_bits_addr),
    .io_nasti_w_ready(iaxi2apb_io_nasti_w_ready),
    .io_nasti_w_valid(iaxi2apb_io_nasti_w_valid),
    .io_nasti_w_bits_data(iaxi2apb_io_nasti_w_bits_data),
    .io_nasti_w_bits_strb(iaxi2apb_io_nasti_w_bits_strb),
    .io_nasti_b_ready(iaxi2apb_io_nasti_b_ready),
    .io_nasti_b_valid(iaxi2apb_io_nasti_b_valid),
    .io_nasti_ar_ready(iaxi2apb_io_nasti_ar_ready),
    .io_nasti_ar_valid(iaxi2apb_io_nasti_ar_valid),
    .io_nasti_ar_bits_addr(iaxi2apb_io_nasti_ar_bits_addr),
    .io_nasti_r_ready(iaxi2apb_io_nasti_r_ready),
    .io_nasti_r_valid(iaxi2apb_io_nasti_r_valid),
    .io_nasti_r_bits_data(iaxi2apb_io_nasti_r_bits_data)
  );
  Mini2axi daxi2apb ( // @[Tile.scala 232:24]
    .clock(daxi2apb_clock),
    .reset(daxi2apb_reset),
    .io_cpu_req_valid(daxi2apb_io_cpu_req_valid),
    .io_cpu_req_bits_addr(daxi2apb_io_cpu_req_bits_addr),
    .io_cpu_req_bits_data(daxi2apb_io_cpu_req_bits_data),
    .io_cpu_req_bits_mask(daxi2apb_io_cpu_req_bits_mask),
    .io_cpu_resp_valid(daxi2apb_io_cpu_resp_valid),
    .io_cpu_resp_bits_data(daxi2apb_io_cpu_resp_bits_data),
    .io_nasti_aw_ready(daxi2apb_io_nasti_aw_ready),
    .io_nasti_aw_valid(daxi2apb_io_nasti_aw_valid),
    .io_nasti_aw_bits_addr(daxi2apb_io_nasti_aw_bits_addr),
    .io_nasti_w_ready(daxi2apb_io_nasti_w_ready),
    .io_nasti_w_valid(daxi2apb_io_nasti_w_valid),
    .io_nasti_w_bits_data(daxi2apb_io_nasti_w_bits_data),
    .io_nasti_w_bits_strb(daxi2apb_io_nasti_w_bits_strb),
    .io_nasti_b_ready(daxi2apb_io_nasti_b_ready),
    .io_nasti_b_valid(daxi2apb_io_nasti_b_valid),
    .io_nasti_ar_ready(daxi2apb_io_nasti_ar_ready),
    .io_nasti_ar_valid(daxi2apb_io_nasti_ar_valid),
    .io_nasti_ar_bits_addr(daxi2apb_io_nasti_ar_bits_addr),
    .io_nasti_r_ready(daxi2apb_io_nasti_r_ready),
    .io_nasti_r_valid(daxi2apb_io_nasti_r_valid),
    .io_nasti_r_bits_data(daxi2apb_io_nasti_r_bits_data)
  );
  Mini2axi uart ( // @[Tile.scala 233:20]
    .clock(uart_clock),
    .reset(uart_reset),
    .io_cpu_req_valid(uart_io_cpu_req_valid),
    .io_cpu_req_bits_addr(uart_io_cpu_req_bits_addr),
    .io_cpu_req_bits_data(uart_io_cpu_req_bits_data),
    .io_cpu_req_bits_mask(uart_io_cpu_req_bits_mask),
    .io_cpu_resp_valid(uart_io_cpu_resp_valid),
    .io_cpu_resp_bits_data(uart_io_cpu_resp_bits_data),
    .io_nasti_aw_ready(uart_io_nasti_aw_ready),
    .io_nasti_aw_valid(uart_io_nasti_aw_valid),
    .io_nasti_aw_bits_addr(uart_io_nasti_aw_bits_addr),
    .io_nasti_w_ready(uart_io_nasti_w_ready),
    .io_nasti_w_valid(uart_io_nasti_w_valid),
    .io_nasti_w_bits_data(uart_io_nasti_w_bits_data),
    .io_nasti_w_bits_strb(uart_io_nasti_w_bits_strb),
    .io_nasti_b_ready(uart_io_nasti_b_ready),
    .io_nasti_b_valid(uart_io_nasti_b_valid),
    .io_nasti_ar_ready(uart_io_nasti_ar_ready),
    .io_nasti_ar_valid(uart_io_nasti_ar_valid),
    .io_nasti_ar_bits_addr(uart_io_nasti_ar_bits_addr),
    .io_nasti_r_ready(uart_io_nasti_r_ready),
    .io_nasti_r_valid(uart_io_nasti_r_valid),
    .io_nasti_r_bits_data(uart_io_nasti_r_bits_data)
  );
  MemArbiter arb ( // @[Tile.scala 234:19]
    .clock(arb_clock),
    .reset(arb_reset),
    .io_icache_ar_ready(arb_io_icache_ar_ready),
    .io_icache_ar_valid(arb_io_icache_ar_valid),
    .io_icache_r_ready(arb_io_icache_r_ready),
    .io_icache_r_valid(arb_io_icache_r_valid),
    .io_icache_r_bits_data(arb_io_icache_r_bits_data),
    .io_dcache_aw_ready(arb_io_dcache_aw_ready),
    .io_dcache_aw_valid(arb_io_dcache_aw_valid),
    .io_dcache_aw_bits_addr(arb_io_dcache_aw_bits_addr),
    .io_dcache_w_ready(arb_io_dcache_w_ready),
    .io_dcache_w_valid(arb_io_dcache_w_valid),
    .io_dcache_w_bits_data(arb_io_dcache_w_bits_data),
    .io_dcache_w_bits_last(arb_io_dcache_w_bits_last),
    .io_dcache_b_ready(arb_io_dcache_b_ready),
    .io_dcache_b_valid(arb_io_dcache_b_valid),
    .io_dcache_ar_ready(arb_io_dcache_ar_ready),
    .io_dcache_ar_valid(arb_io_dcache_ar_valid),
    .io_dcache_r_ready(arb_io_dcache_r_ready),
    .io_dcache_r_valid(arb_io_dcache_r_valid),
    .io_dcache_r_bits_data(arb_io_dcache_r_bits_data),
    .io_iaxi2apb_ar_ready(arb_io_iaxi2apb_ar_ready),
    .io_iaxi2apb_ar_valid(arb_io_iaxi2apb_ar_valid),
    .io_iaxi2apb_ar_bits_addr(arb_io_iaxi2apb_ar_bits_addr),
    .io_iaxi2apb_r_ready(arb_io_iaxi2apb_r_ready),
    .io_iaxi2apb_r_valid(arb_io_iaxi2apb_r_valid),
    .io_iaxi2apb_r_bits_data(arb_io_iaxi2apb_r_bits_data),
    .io_daxi2apb_aw_ready(arb_io_daxi2apb_aw_ready),
    .io_daxi2apb_aw_valid(arb_io_daxi2apb_aw_valid),
    .io_daxi2apb_aw_bits_addr(arb_io_daxi2apb_aw_bits_addr),
    .io_daxi2apb_w_ready(arb_io_daxi2apb_w_ready),
    .io_daxi2apb_w_valid(arb_io_daxi2apb_w_valid),
    .io_daxi2apb_w_bits_data(arb_io_daxi2apb_w_bits_data),
    .io_daxi2apb_w_bits_strb(arb_io_daxi2apb_w_bits_strb),
    .io_daxi2apb_b_ready(arb_io_daxi2apb_b_ready),
    .io_daxi2apb_b_valid(arb_io_daxi2apb_b_valid),
    .io_daxi2apb_ar_ready(arb_io_daxi2apb_ar_ready),
    .io_daxi2apb_ar_valid(arb_io_daxi2apb_ar_valid),
    .io_daxi2apb_ar_bits_addr(arb_io_daxi2apb_ar_bits_addr),
    .io_daxi2apb_r_ready(arb_io_daxi2apb_r_ready),
    .io_daxi2apb_r_valid(arb_io_daxi2apb_r_valid),
    .io_daxi2apb_r_bits_data(arb_io_daxi2apb_r_bits_data),
    .io_uart_aw_ready(arb_io_uart_aw_ready),
    .io_uart_aw_valid(arb_io_uart_aw_valid),
    .io_uart_aw_bits_addr(arb_io_uart_aw_bits_addr),
    .io_uart_w_ready(arb_io_uart_w_ready),
    .io_uart_w_valid(arb_io_uart_w_valid),
    .io_uart_w_bits_data(arb_io_uart_w_bits_data),
    .io_uart_w_bits_strb(arb_io_uart_w_bits_strb),
    .io_uart_b_ready(arb_io_uart_b_ready),
    .io_uart_b_valid(arb_io_uart_b_valid),
    .io_uart_ar_ready(arb_io_uart_ar_ready),
    .io_uart_ar_valid(arb_io_uart_ar_valid),
    .io_uart_ar_bits_addr(arb_io_uart_ar_bits_addr),
    .io_uart_r_ready(arb_io_uart_r_ready),
    .io_uart_r_valid(arb_io_uart_r_valid),
    .io_uart_r_bits_data(arb_io_uart_r_bits_data),
    .io_nasti_aw_ready(arb_io_nasti_aw_ready),
    .io_nasti_aw_valid(arb_io_nasti_aw_valid),
    .io_nasti_aw_bits_addr(arb_io_nasti_aw_bits_addr),
    .io_nasti_aw_bits_len(arb_io_nasti_aw_bits_len),
    .io_nasti_aw_bits_size(arb_io_nasti_aw_bits_size),
    .io_nasti_w_ready(arb_io_nasti_w_ready),
    .io_nasti_w_valid(arb_io_nasti_w_valid),
    .io_nasti_w_bits_data(arb_io_nasti_w_bits_data),
    .io_nasti_w_bits_strb(arb_io_nasti_w_bits_strb),
    .io_nasti_w_bits_last(arb_io_nasti_w_bits_last),
    .io_nasti_b_ready(arb_io_nasti_b_ready),
    .io_nasti_b_valid(arb_io_nasti_b_valid),
    .io_nasti_ar_ready(arb_io_nasti_ar_ready),
    .io_nasti_ar_valid(arb_io_nasti_ar_valid),
    .io_nasti_ar_bits_addr(arb_io_nasti_ar_bits_addr),
    .io_nasti_ar_bits_len(arb_io_nasti_ar_bits_len),
    .io_nasti_ar_bits_size(arb_io_nasti_ar_bits_size),
    .io_nasti_r_ready(arb_io_nasti_r_ready),
    .io_nasti_r_valid(arb_io_nasti_r_valid),
    .io_nasti_r_bits_data(arb_io_nasti_r_bits_data),
    .io_nasti_r_bits_last(arb_io_nasti_r_bits_last)
  );
  assign io_host_tohost = core_io_host_tohost; // @[Tile.scala 236:11]
  assign io_nasti_aw_valid = arb_io_nasti_aw_valid; // @[Tile.scala 247:12]
  assign io_nasti_aw_bits_id = 4'h0; // @[Tile.scala 247:12]
  assign io_nasti_aw_bits_addr = arb_io_nasti_aw_bits_addr; // @[Tile.scala 247:12]
  assign io_nasti_aw_bits_len = arb_io_nasti_aw_bits_len; // @[Tile.scala 247:12]
  assign io_nasti_aw_bits_size = arb_io_nasti_aw_bits_size; // @[Tile.scala 247:12]
  assign io_nasti_aw_bits_burst = 2'h1; // @[Tile.scala 247:12]
  assign io_nasti_aw_bits_lock = 1'h0; // @[Tile.scala 247:12]
  assign io_nasti_aw_bits_cache = 4'h0; // @[Tile.scala 247:12]
  assign io_nasti_aw_bits_prot = 3'h0; // @[Tile.scala 247:12]
  assign io_nasti_aw_bits_qos = 4'h0; // @[Tile.scala 247:12]
  assign io_nasti_w_valid = arb_io_nasti_w_valid; // @[Tile.scala 247:12]
  assign io_nasti_w_bits_data = arb_io_nasti_w_bits_data; // @[Tile.scala 247:12]
  assign io_nasti_w_bits_strb = arb_io_nasti_w_bits_strb; // @[Tile.scala 247:12]
  assign io_nasti_w_bits_last = arb_io_nasti_w_bits_last; // @[Tile.scala 247:12]
  assign io_nasti_b_ready = arb_io_nasti_b_ready; // @[Tile.scala 247:12]
  assign io_nasti_ar_valid = arb_io_nasti_ar_valid; // @[Tile.scala 247:12]
  assign io_nasti_ar_bits_id = 4'h0; // @[Tile.scala 247:12]
  assign io_nasti_ar_bits_addr = arb_io_nasti_ar_bits_addr; // @[Tile.scala 247:12]
  assign io_nasti_ar_bits_len = arb_io_nasti_ar_bits_len; // @[Tile.scala 247:12]
  assign io_nasti_ar_bits_size = arb_io_nasti_ar_bits_size; // @[Tile.scala 247:12]
  assign io_nasti_ar_bits_burst = 2'h1; // @[Tile.scala 247:12]
  assign io_nasti_ar_bits_lock = 1'h0; // @[Tile.scala 247:12]
  assign io_nasti_ar_bits_cache = 4'h0; // @[Tile.scala 247:12]
  assign io_nasti_ar_bits_prot = 3'h0; // @[Tile.scala 247:12]
  assign io_nasti_ar_bits_qos = 4'h0; // @[Tile.scala 247:12]
  assign io_nasti_r_ready = arb_io_nasti_r_ready; // @[Tile.scala 247:12]
  assign core_clock = clock;
  assign core_reset = reset;
  assign core_io_host_fromhost_valid = io_host_fromhost_valid; // @[Tile.scala 236:11]
  assign core_io_host_fromhost_bits = io_host_fromhost_bits; // @[Tile.scala 236:11]
  assign core_io_icache_resp_valid = icache_io_cpu_resp_valid; // @[Tile.scala 237:18]
  assign core_io_icache_resp_bits_data = icache_io_cpu_resp_bits_data; // @[Tile.scala 237:18]
  assign core_io_dcache_resp_valid = dcache_io_cpu_resp_valid; // @[Tile.scala 238:18]
  assign core_io_dcache_resp_bits_data = dcache_io_cpu_resp_bits_data; // @[Tile.scala 238:18]
  assign core_io_iaxi2apb_resp_valid = iaxi2apb_io_cpu_resp_valid; // @[Tile.scala 239:20]
  assign core_io_iaxi2apb_resp_bits_data = iaxi2apb_io_cpu_resp_bits_data; // @[Tile.scala 239:20]
  assign core_io_daxi2apb_resp_valid = daxi2apb_io_cpu_resp_valid; // @[Tile.scala 240:20]
  assign core_io_daxi2apb_resp_bits_data = daxi2apb_io_cpu_resp_bits_data; // @[Tile.scala 240:20]
  assign core_io_uart_resp_valid = uart_io_cpu_resp_valid; // @[Tile.scala 241:16]
  assign icache_clock = clock;
  assign icache_reset = reset;
  assign icache_io_cpu_abort = 1'h0; // @[Tile.scala 237:18]
  assign icache_io_nasti_aw_ready = 1'h0; // @[Tile.scala 242:17]
  assign icache_io_nasti_w_ready = 1'h0; // @[Tile.scala 242:17]
  assign icache_io_nasti_b_valid = 1'h0; // @[Tile.scala 242:17]
  assign icache_io_nasti_ar_ready = arb_io_icache_ar_ready; // @[Tile.scala 242:17]
  assign icache_io_nasti_r_valid = arb_io_icache_r_valid; // @[Tile.scala 242:17]
  assign icache_io_nasti_r_bits_data = arb_io_icache_r_bits_data; // @[Tile.scala 242:17]
  assign dcache_clock = clock;
  assign dcache_reset = reset;
  assign dcache_io_cpu_abort = core_io_dcache_abort; // @[Tile.scala 238:18]
  assign dcache_io_nasti_aw_ready = arb_io_dcache_aw_ready; // @[Tile.scala 243:17]
  assign dcache_io_nasti_w_ready = arb_io_dcache_w_ready; // @[Tile.scala 243:17]
  assign dcache_io_nasti_b_valid = arb_io_dcache_b_valid; // @[Tile.scala 243:17]
  assign dcache_io_nasti_ar_ready = arb_io_dcache_ar_ready; // @[Tile.scala 243:17]
  assign dcache_io_nasti_r_valid = arb_io_dcache_r_valid; // @[Tile.scala 243:17]
  assign dcache_io_nasti_r_bits_data = arb_io_dcache_r_bits_data; // @[Tile.scala 243:17]
  assign iaxi2apb_clock = clock;
  assign iaxi2apb_reset = reset;
  assign iaxi2apb_io_cpu_req_valid = core_io_iaxi2apb_req_valid; // @[Tile.scala 239:20]
  assign iaxi2apb_io_cpu_req_bits_addr = core_io_iaxi2apb_req_bits_addr; // @[Tile.scala 239:20]
  assign iaxi2apb_io_cpu_req_bits_data = 32'h0; // @[Tile.scala 239:20]
  assign iaxi2apb_io_cpu_req_bits_mask = 4'h0; // @[Tile.scala 239:20]
  assign iaxi2apb_io_nasti_aw_ready = 1'h0; // @[Tile.scala 244:19]
  assign iaxi2apb_io_nasti_w_ready = 1'h0; // @[Tile.scala 244:19]
  assign iaxi2apb_io_nasti_b_valid = 1'h0; // @[Tile.scala 244:19]
  assign iaxi2apb_io_nasti_ar_ready = arb_io_iaxi2apb_ar_ready; // @[Tile.scala 244:19]
  assign iaxi2apb_io_nasti_r_valid = arb_io_iaxi2apb_r_valid; // @[Tile.scala 244:19]
  assign iaxi2apb_io_nasti_r_bits_data = arb_io_iaxi2apb_r_bits_data; // @[Tile.scala 244:19]
  assign daxi2apb_clock = clock;
  assign daxi2apb_reset = reset;
  assign daxi2apb_io_cpu_req_valid = core_io_daxi2apb_req_valid; // @[Tile.scala 240:20]
  assign daxi2apb_io_cpu_req_bits_addr = core_io_daxi2apb_req_bits_addr; // @[Tile.scala 240:20]
  assign daxi2apb_io_cpu_req_bits_data = core_io_daxi2apb_req_bits_data; // @[Tile.scala 240:20]
  assign daxi2apb_io_cpu_req_bits_mask = core_io_daxi2apb_req_bits_mask; // @[Tile.scala 240:20]
  assign daxi2apb_io_nasti_aw_ready = arb_io_daxi2apb_aw_ready; // @[Tile.scala 245:19]
  assign daxi2apb_io_nasti_w_ready = arb_io_daxi2apb_w_ready; // @[Tile.scala 245:19]
  assign daxi2apb_io_nasti_b_valid = arb_io_daxi2apb_b_valid; // @[Tile.scala 245:19]
  assign daxi2apb_io_nasti_ar_ready = arb_io_daxi2apb_ar_ready; // @[Tile.scala 245:19]
  assign daxi2apb_io_nasti_r_valid = arb_io_daxi2apb_r_valid; // @[Tile.scala 245:19]
  assign daxi2apb_io_nasti_r_bits_data = arb_io_daxi2apb_r_bits_data; // @[Tile.scala 245:19]
  assign uart_clock = clock;
  assign uart_reset = reset;
  assign uart_io_cpu_req_valid = 1'h0; // @[Tile.scala 241:16]
  assign uart_io_cpu_req_bits_addr = 32'h0; // @[Tile.scala 241:16]
  assign uart_io_cpu_req_bits_data = 32'h0; // @[Tile.scala 241:16]
  assign uart_io_cpu_req_bits_mask = 4'h0; // @[Tile.scala 241:16]
  assign uart_io_nasti_aw_ready = arb_io_uart_aw_ready; // @[Tile.scala 246:15]
  assign uart_io_nasti_w_ready = arb_io_uart_w_ready; // @[Tile.scala 246:15]
  assign uart_io_nasti_b_valid = arb_io_uart_b_valid; // @[Tile.scala 246:15]
  assign uart_io_nasti_ar_ready = arb_io_uart_ar_ready; // @[Tile.scala 246:15]
  assign uart_io_nasti_r_valid = arb_io_uart_r_valid; // @[Tile.scala 246:15]
  assign uart_io_nasti_r_bits_data = arb_io_uart_r_bits_data; // @[Tile.scala 246:15]
  assign arb_clock = clock;
  assign arb_reset = reset;
  assign arb_io_icache_ar_valid = icache_io_nasti_ar_valid; // @[Tile.scala 242:17]
  assign arb_io_icache_r_ready = icache_io_nasti_r_ready; // @[Tile.scala 242:17]
  assign arb_io_dcache_aw_valid = dcache_io_nasti_aw_valid; // @[Tile.scala 243:17]
  assign arb_io_dcache_aw_bits_addr = dcache_io_nasti_aw_bits_addr; // @[Tile.scala 243:17]
  assign arb_io_dcache_w_valid = dcache_io_nasti_w_valid; // @[Tile.scala 243:17]
  assign arb_io_dcache_w_bits_data = dcache_io_nasti_w_bits_data; // @[Tile.scala 243:17]
  assign arb_io_dcache_w_bits_last = dcache_io_nasti_w_bits_last; // @[Tile.scala 243:17]
  assign arb_io_dcache_b_ready = dcache_io_nasti_b_ready; // @[Tile.scala 243:17]
  assign arb_io_dcache_ar_valid = dcache_io_nasti_ar_valid; // @[Tile.scala 243:17]
  assign arb_io_dcache_r_ready = dcache_io_nasti_r_ready; // @[Tile.scala 243:17]
  assign arb_io_iaxi2apb_ar_valid = iaxi2apb_io_nasti_ar_valid; // @[Tile.scala 244:19]
  assign arb_io_iaxi2apb_ar_bits_addr = iaxi2apb_io_nasti_ar_bits_addr; // @[Tile.scala 244:19]
  assign arb_io_iaxi2apb_r_ready = iaxi2apb_io_nasti_r_ready; // @[Tile.scala 244:19]
  assign arb_io_daxi2apb_aw_valid = daxi2apb_io_nasti_aw_valid; // @[Tile.scala 245:19]
  assign arb_io_daxi2apb_aw_bits_addr = daxi2apb_io_nasti_aw_bits_addr; // @[Tile.scala 245:19]
  assign arb_io_daxi2apb_w_valid = daxi2apb_io_nasti_w_valid; // @[Tile.scala 245:19]
  assign arb_io_daxi2apb_w_bits_data = daxi2apb_io_nasti_w_bits_data; // @[Tile.scala 245:19]
  assign arb_io_daxi2apb_w_bits_strb = daxi2apb_io_nasti_w_bits_strb; // @[Tile.scala 245:19]
  assign arb_io_daxi2apb_b_ready = daxi2apb_io_nasti_b_ready; // @[Tile.scala 245:19]
  assign arb_io_daxi2apb_ar_valid = daxi2apb_io_nasti_ar_valid; // @[Tile.scala 245:19]
  assign arb_io_daxi2apb_ar_bits_addr = daxi2apb_io_nasti_ar_bits_addr; // @[Tile.scala 245:19]
  assign arb_io_daxi2apb_r_ready = daxi2apb_io_nasti_r_ready; // @[Tile.scala 245:19]
  assign arb_io_uart_aw_valid = uart_io_nasti_aw_valid; // @[Tile.scala 246:15]
  assign arb_io_uart_aw_bits_addr = uart_io_nasti_aw_bits_addr; // @[Tile.scala 246:15]
  assign arb_io_uart_w_valid = uart_io_nasti_w_valid; // @[Tile.scala 246:15]
  assign arb_io_uart_w_bits_data = uart_io_nasti_w_bits_data; // @[Tile.scala 246:15]
  assign arb_io_uart_w_bits_strb = uart_io_nasti_w_bits_strb; // @[Tile.scala 246:15]
  assign arb_io_uart_b_ready = uart_io_nasti_b_ready; // @[Tile.scala 246:15]
  assign arb_io_uart_ar_valid = uart_io_nasti_ar_valid; // @[Tile.scala 246:15]
  assign arb_io_uart_ar_bits_addr = uart_io_nasti_ar_bits_addr; // @[Tile.scala 246:15]
  assign arb_io_uart_r_ready = uart_io_nasti_r_ready; // @[Tile.scala 246:15]
  assign arb_io_nasti_aw_ready = io_nasti_aw_ready; // @[Tile.scala 247:12]
  assign arb_io_nasti_w_ready = io_nasti_w_ready; // @[Tile.scala 247:12]
  assign arb_io_nasti_b_valid = io_nasti_b_valid; // @[Tile.scala 247:12]
  assign arb_io_nasti_ar_ready = io_nasti_ar_ready; // @[Tile.scala 247:12]
  assign arb_io_nasti_r_valid = io_nasti_r_valid; // @[Tile.scala 247:12]
  assign arb_io_nasti_r_bits_data = io_nasti_r_bits_data; // @[Tile.scala 247:12]
  assign arb_io_nasti_r_bits_last = io_nasti_r_bits_last; // @[Tile.scala 247:12]
endmodule

module ysyx_mini (
  input          clock,
  input          reset,
  input          io_master_arready,
  output         io_master_arvalid,
  output [31:0]  io_master_araddr,
  output [3:0]   io_master_arid,
  output [7:0]   io_master_arlen,
  output [2:0]   io_master_arsize,
  output [1:0]   io_master_arburst,
  output         io_master_rready,
  input          io_master_rvalid,
  input  [1:0]   io_master_rresp,
  input  [63:0]  io_master_rdata,
  input          io_master_rlast,
  input  [3:0]   io_master_rid,
  input          io_master_awready,
  output         io_master_awvalid,
  output [31:0]  io_master_awaddr,
  output [3:0]   io_master_awid,
  output [7:0]   io_master_awlen,
  output [2:0]   io_master_awsize,
  output [1:0]   io_master_awburst,
  input          io_master_wready,
  output         io_master_wvalid,
  output [63:0]  io_master_wdata,
  output [7:0]   io_master_wstrb,
  output         io_master_wlast,
  output         io_master_bready,
  input          io_master_bvalid,
  input  [1:0]   io_master_bresp,
  input  [3:0]   io_master_bid,
  output         io_slave_arready,
  input          io_slave_arvalid,
  input  [31:0]  io_slave_araddr,
  input  [3:0]   io_slave_arid,
  input  [7:0]   io_slave_arlen,
  input  [2:0]   io_slave_arsize,
  input  [1:0]   io_slave_arburst,
  input          io_slave_rready,
  output         io_slave_rvalid,
  output [1:0]   io_slave_rresp,
  output [63:0]  io_slave_rdata,
  output         io_slave_rlast,
  output [3:0]   io_slave_rid,
  output         io_slave_awready,
  input          io_slave_awvalid,
  input  [31:0]  io_slave_awaddr,
  input  [3:0]   io_slave_awid,
  input  [7:0]   io_slave_awlen,
  input  [2:0]   io_slave_awsize,
  input  [1:0]   io_slave_awburst,
  output         io_slave_wready,
  input          io_slave_wvalid,
  input  [63:0]  io_slave_wdata,
  input  [7:0]   io_slave_wstrb,
  input          io_slave_wlast,
  input          io_slave_bready,
  output         io_slave_bvalid,
  output [1:0]   io_slave_bresp,
  output [3:0]   io_slave_bid,
  input          io_interrupt,
  output [5:0]   io_sram0_addr,
  output         io_sram0_cen,
  output         io_sram0_wen,
  output [127:0] io_sram0_wmask,
  output [127:0] io_sram0_wdata,
  input  [127:0] io_sram0_rdata,
  output [5:0]   io_sram1_addr,
  output         io_sram1_cen,
  output         io_sram1_wen,
  output [127:0] io_sram1_wmask,
  output [127:0] io_sram1_wdata,
  input  [127:0] io_sram1_rdata,
  output [5:0]   io_sram2_addr,
  output         io_sram2_cen,
  output         io_sram2_wen,
  output [127:0] io_sram2_wmask,
  output [127:0] io_sram2_wdata,
  input  [127:0] io_sram2_rdata,
  output [5:0]   io_sram3_addr,
  output         io_sram3_cen,
  output         io_sram3_wen,
  output [127:0] io_sram3_wmask,
  output [127:0] io_sram3_wdata,
  input  [127:0] io_sram3_rdata,
  output [5:0]   io_sram4_addr,
  output         io_sram4_cen,
  output         io_sram4_wen,
  output [127:0] io_sram4_wmask,
  output [127:0] io_sram4_wdata,
  input  [127:0] io_sram4_rdata,
  output [5:0]   io_sram5_addr,
  output         io_sram5_cen,
  output         io_sram5_wen,
  output [127:0] io_sram5_wmask,
  output [127:0] io_sram5_wdata,
  input  [127:0] io_sram5_rdata,
  output [5:0]   io_sram6_addr,
  output         io_sram6_cen,
  output         io_sram6_wen,
  output [127:0] io_sram6_wmask,
  output [127:0] io_sram6_wdata,
  input  [127:0] io_sram6_rdata,
  output [5:0]   io_sram7_addr,
  output         io_sram7_cen,
  output         io_sram7_wen,
  output [127:0] io_sram7_wmask,
  output [127:0] io_sram7_wdata,
  input  [127:0] io_sram7_rdata
);

  assign io_slave_awready = 0;
  assign io_slave_wready = 0;
  assign io_slave_bvalid = 0;
  assign io_slave_bresp = 0;
  assign io_slave_bid = 0;
  assign io_slave_arready = 0;
  assign io_slave_rvalid = 0;
  assign io_slave_rresp = 0;
  assign io_slave_rdata = 0;
  assign io_slave_rlast = 0;
  assign io_slave_rid = 0;

  wire io_host_fromhost_valid = 1'b0;
  wire [31: 0] io_host_fromhost_bits  = 32'b0;


Tile u_Tile(
  .clock(clock),
  .reset(reset),
  .io_host_fromhost_valid(io_host_fromhost_valid),
  .io_host_fromhost_bits(io_host_fromhost_bits),
  .io_host_tohost(),

  .io_nasti_aw_ready        (io_master_awready),
  .io_nasti_aw_valid        (io_master_awvalid),
  .io_nasti_aw_bits_id      (io_master_awid),
  .io_nasti_aw_bits_addr    (io_master_awaddr),
  .io_nasti_aw_bits_len     (io_master_awlen  ),
  .io_nasti_aw_bits_size    (io_master_awsize ),
  .io_nasti_aw_bits_burst   (io_master_awburst),
  .io_nasti_aw_bits_lock    ( ),
  .io_nasti_aw_bits_cache   (),
  .io_nasti_aw_bits_prot    ( ),
  .io_nasti_aw_bits_qos     (),
  .io_nasti_w_ready         (io_master_wready ),
  .io_nasti_w_valid         (io_master_wvalid ),
  .io_nasti_w_bits_data     (io_master_wdata  ),
  .io_nasti_w_bits_strb     (io_master_wstrb  ),
  .io_nasti_w_bits_last     (io_master_wlast  ),
  .io_nasti_b_ready         (io_master_bready ),
  .io_nasti_b_valid         (io_master_bvalid ),
  .io_nasti_b_bits_id        (io_master_bid),
  .io_nasti_b_bits_resp     (io_master_bresp  ),
  
  .io_nasti_ar_ready        (io_master_arready),
  .io_nasti_ar_valid        (io_master_arvalid),
  .io_nasti_ar_bits_id      (io_master_arid),  
  .io_nasti_ar_bits_addr    (io_master_araddr ),
  .io_nasti_ar_bits_len     (io_master_arlen  ),
  .io_nasti_ar_bits_size    (io_master_arsize ),
  .io_nasti_ar_bits_burst   (io_master_arburst),
  .io_nasti_ar_bits_lock    ( ),
  .io_nasti_ar_bits_cache   (),
  .io_nasti_ar_bits_prot    ( ),
  .io_nasti_ar_bits_qos     (),
  .io_nasti_r_ready         (io_master_rready ),
  .io_nasti_r_valid         (io_master_rvalid ),
  .io_nasti_r_bits_id       (io_master_rid),
  .io_nasti_r_bits_data     (io_master_rdata  ),
  .io_nasti_r_bits_resp     (io_master_rresp  ),
  .io_nasti_r_bits_last     (io_master_rlast  )
 
);

endmodule

