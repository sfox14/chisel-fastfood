// the dual-port BRAM Verilog below is adapted from Dan Strother's example:
// http://danstrother.com/2010/09/11/inferring-rams-in-fpgas/
// incl: doA and doB pipeline registers

module DualPortBRAM #(
    parameter DATA = 72,
    parameter ADDR = 10,
    parameter FNAME = "pe_0.mem"
) (
    input   wire               clk,
    // Port A
    input   wire                a_wr,
    input   wire    [ADDR-1:0]  a_addr,
    input   wire    [DATA-1:0]  a_din,
    output  reg     [DATA-1:0]  a_dout,
    // Port B
    input   wire                b_wr,
    input   wire    [ADDR-1:0]  b_addr,
    input   wire    [DATA-1:0]  b_din,
    output  reg     [DATA-1:0]  b_dout
);
// Shared memory
reg [DATA-1:0] mem [(2**ADDR)-1:0];
reg [DATA-1:0] doA, doB; // output regusters

//initial
//begin
//    $readmemh(FNAME, mem);
//end

// Port A
always @(posedge clk) begin
    doA     <= mem[a_addr];
    a_dout  <= doA;  
    if(a_wr) begin
        a_dout      <= a_din;
        mem[a_addr] <= a_din;
    end
end
// Port B
always @(posedge clk) begin
    doB     <= mem[b_addr];
    b_dout  <= doB;
    if(b_wr) begin
        b_dout      <= b_din;
        mem[b_addr] <= b_din;
    end
end
endmodule
