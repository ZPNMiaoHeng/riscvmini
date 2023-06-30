
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

