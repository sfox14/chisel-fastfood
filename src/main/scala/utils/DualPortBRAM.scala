package utils

import Chisel._
import java.io._
import math._
// A module for inferring true dual-pPort BRAMs on FPGAs
// Taken from git://github.com/maltanar/fpga-tidbits.git and edited for any Fixed type


class OCMRequest[T <: Data](gen : T, addrWidth: Int) extends Bundle {
  val addr = UInt(width = addrWidth)
  val writeData = gen.cloneType //UInt(width = writeWidth)
  val writeEn = Bool()

  override def clone = {new OCMRequest(gen.cloneType, addrWidth).asInstanceOf[this.type]}

}
class OCMResponse[T <: Data](gen : T) extends Bundle {
  val readData = gen.cloneType //UInt(width = readWidth)

  override def clone = {new OCMResponse( gen.cloneType ).asInstanceOf[this.type]}

}
// slave interface is just the master interface flipped
class OCMSlaveIF[T <: Data](gen : T, addrWidth: Int) extends Bundle {
  val req = new OCMRequest(gen, addrWidth).asInput()
  val rsp = new OCMResponse(gen).asOutput()
}


// Since (Xilinx) FPGA synthesis tools do not infer TDP BRAMs from
// Chisel-generated Verilog (both ports in the same "always" block),
// we use a BlackBox with a premade Verilog BRAM template.

class DualPortBRAMIO[T <: Data](gen : T, addrBits: Int) extends Bundle {
  val ports = Vec.fill (2) {new OCMSlaveIF(gen, addrBits)}

  ports(0).req.addr.setName("a_addr")
  ports(0).req.writeData.setName("a_din")
  ports(0).req.writeEn.setName("a_wr")
  ports(0).rsp.readData.setName("a_dout")

  ports(1).req.addr.setName("b_addr")
  ports(1).req.writeData.setName("b_din")
  ports(1).req.writeEn.setName("b_wr")
  ports(1).rsp.readData.setName("b_dout")
}

// For higher fmax
class PipelinedDualPortBRAM[T <: Data](gen : T, addrBits: Int,
  regIn: Int,    // number of registers at input
  regOut: Int,   // number of registers at output
  id : Int, init : Seq[BigInt], forSim : Boolean = true
) extends Module {


  val io = new DualPortBRAMIO(gen, addrBits)
  // instantiate the desired BRAM
  val bram = Module(new DualPortBRAM(gen, addrBits, id, init, forSim)).io

  bram.ports(0).req := ShiftRegister(io.ports(0).req, regIn)
  bram.ports(1).req := ShiftRegister(io.ports(1).req, regIn)

  io.ports(0).rsp := ShiftRegister(bram.ports(0).rsp, regOut)
  io.ports(1).rsp := ShiftRegister(bram.ports(1).rsp, regOut)
}

class DualPortBRAM[T <: Data](gen : T, addrBits: Int, id : Int,  
    init : Seq[BigInt], forSim : Boolean = true ) extends BlackBox {
  val io = new DualPortBRAMIO(gen, addrBits)
  setVerilogParameters(new VerilogParameters {
    val DATA = gen.getWidth
    val ADDR = addrBits
    val FNAME = s"pe_$id.data"
  })

  // the clock does not get added to the BlackBox interface by default
  addClock( Driver.implicitClock )

  // simulation model for TDP BRAM
  //val mem = Mem(gen.cloneType, 1 << addrBits)
  println( log2Up( init.length ), addrBits )
  Predef.assert( log2Up( init.length ) == addrBits, s"Error: Missing BRAM init values" )

  //Write init data to a file
  if( !forSim ){
    toFile(id, init)
  }

  // Chisel Mem does not accept an initialisation. Instead use Vec(RegInit) but only 
  // for simulation, otherwise the verilog instantiation will have a reset.
  if( forSim ){
    //println("simulating...")
    val ram = Vec( init.map((i : BigInt) => Fixed(i, 18, 10) ) )
    val mem = RegInit( ram )

    for (i <- 0 until 2) {
      val req = io.ports(i).req
      val regAddr = Reg(next = io.ports(i).req.addr)

      io.ports(i).rsp.readData := mem(regAddr)

      when (req.writeEn) {
        mem(req.addr) := req.writeData
      }
    }
  }

}

/*
Save contents of BRAM to file, and ensure the formatting supports XST 
*/
object toFile{

  def apply( id : Int, data : Seq[BigInt] ){

    val file = s".temp_mem/pe_$id.mem"
    val writer = new BufferedWriter( new OutputStreamWriter( new FileOutputStream(file) ) )
    for( x <- format(data) ){
      writer.write( x + "\n")
    }
    writer.close()
  }

  def format( data : Seq[BigInt] ) : Seq[String] = {
    // 18-bits, signed
    var bw = 18

    var zeros = List.fill(bw)("0").mkString("") //"000000000000000000" //18 zeros
    var combinedBits = ""
    for( x <- data ){
      var hexString = ( Literal(x, bw, true)  // Chisel literal
        .toString           // toString
        .substring(2)       // drop "0x"
        .reverse ++ "000000000" ) // reverse and add zeros
        .substring(0, ceil(bw/4).toInt )     // only want ceil(18/4) bits
        .reverse            // reverse back

      var bitString = ( BigInt( hexString, 16 ).toString(2)
                        .reverse ++ zeros ) //to make sure we get 18-bits
                        .substring(0, bw)
                        .reverse
      combinedBits = combinedBits ++ bitString
    }

    //Predef.assert( combinedBits.length <= 18000, "Error: MEM too big for one BRAM")

    // byte alignment
    (combinedBits map(_.asDigit) ) // big bit string to vector
      .grouped( 16 ).toVector      // group vector to 2 bytes vectors
      .map(x => x.mkString("") )   // make a string from 2 byte vectors
      .map( x => BigInt(x, 2).toString(16) )
      .map( x => (x.reverse ++ "0000").substring(0,4).reverse ) // convert to hex
  } 

}

// the dual-port BRAM Verilog below is adapted from Dan Strother's example:
// http://danstrother.com/2010/09/11/inferring-rams-in-fpgas/
/*

module DualPortBRAM #(
    parameter DATA = 72,
    parameter ADDR = 10
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

// Port A
always @(posedge clk) begin
    a_dout      <= mem[a_addr];
    if(a_wr) begin
        a_dout      <= a_din;
        mem[a_addr] <= a_din;
    end
end

// Port B
always @(posedge clk) begin
    b_dout      <= mem[b_addr];
    if(b_wr) begin
        b_dout      <= b_din;
        mem[b_addr] <= b_din;
    end
end

endmodule
*/
