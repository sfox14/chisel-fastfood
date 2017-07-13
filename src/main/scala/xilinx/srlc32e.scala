package xilinx

import Chisel._


/*
Wrapper for the SRLC32E Shift-Register LUT primitive from Xilinx 
*/

class SRLC32E_IO extends Bundle {
  val dIn = Bool( INPUT )
  val vld = Bool( INPUT )
  val len = UInt( INPUT, width=5 )
  val dOut = Bool( OUTPUT )
  val dCas = Bool( OUTPUT )

  def setNames(){
    dIn.setName("D")
    vld.setName("CE")
    len.setName("A")
    dOut.setName("Q")
    dCas.setName("Q31")
  }

}

/*
1-bit Shift Register, up to 32-bit depth, using Xilinx SRL primitive
*/
class SRLC32E( val hex : String, val len : Int, forSim : Boolean = true ) extends BlackBox {
  
  val io = new SRLC32E_IO
  io.setNames()

  // the clock does not get added to the BlackBox interface by default
  addClock( Driver.implicitClock )
  renameClock( Driver.implicitClock, "CLK" )

  setVerilogParameters(
    "#( .INIT(32'h" + hex + ") )\n"
    )
  setModuleName("SRLC32E")

  if( forSim ){ 
    // Test the module
    val init = BigInt( hex, 16 ).toString(2)                    // from hex string to bin string
    val mem = ( init map(_.asDigit)).toList.reverse.padTo(32,0) // from string to int

    // initialise the registers
    val sr = Vec( Range(0, len, 1).reverse.map( x => RegInit( UInt( mem(x), 1 ) ) ) )

    // connect the registers for the shift
    io.dOut := UInt(0)
    io.dCas := UInt(0)
    when( io.vld ){
      sr(0) := io.dIn
      for (idx <- 0 until len-1){ sr(idx+1) := sr(idx) }
      io.dOut := sr(len-1)
      io.dCas := sr(len-1)
    } 
  }
}

/*
1-bit Shift Register with RegInits for initialisation
*/
class ShiftRegInit( val hex : String, val len : Int ) extends Module {
  
  val io = new SRLC32E_IO

  // Test the module
  val init = BigInt( hex, 16 ).toString(2) 			                // from hex string to bin string
  val mem = ( init map(_.asDigit)).toList.reverse.padTo(32,0) 	// from string to int

  // initialise the registers
  val sr = Vec( Range(0, len, 1).reverse.map( x => RegInit( UInt( mem(x), 1 ) ) ) )

  // connect the registers for the shift
  io.dOut := UInt(0)
  io.dCas := UInt(0)
  when( io.vld ){
    sr(0) := io.dIn
    for (idx <- 0 until len-1){ sr(idx+1) := sr(idx) }
    io.dOut := sr(len-1)
    io.dCas := sr(len-1)
  } 
}

/*
Top module to generate the verilog
*/
class SRLC32E_top extends Module {
  val io = new Bundle {
    val a = Bool( INPUT )
    val b = Bool( OUTPUT )
  }

  val len = 32
  val srl = Module( new SRLC32E( "0000000", len, false ) )
  srl.io.dIn := io.a
  srl.io.len := UInt(len-1, width=5)
  srl.io.vld := Bool(true)
  io.b := srl.io.dOut

}

/*
Generate verilog
*/
object SRLC32EVerilog {

  def main(args: Array[String]): Unit = {
    println("Generating verilog for fastfood module")
    chiselMain(Array("--backend", "v", "--targetDir", "verilog"), 
                () => Module( new SRLC32E_top ) ) 

  }
}
