package xilinx

import Chisel._


/** Wrapper for the SRLC32E Shift-Register LUT primitive from Xilinx
  */

/*
class SrlParams( init : String ) extends VerilogParameters {
  val INIT = init
}*/


// this module is used to used to test SRLC32E primitive
class SRL16ETester[ T <: Data ]( hex : String, len : Int ) extends Module{

  val io = new Bundle{
    val dIn = Bool(INPUT)
    val vld = Bool(INPUT)
    val dOut = Bool(OUTPUT)
  }

  // some scala stuff
  val init = BigInt( hex, 16 ).toString(2) // from hex string to bin string
  val mem = ( init map(_.asDigit)).toList.reverse.padTo(16,0) // from string to int

  // mem is padded with zeros, but then we only use the first #len bit

  // initialise the registers
  val sr = Vec( Range(0, len, 1).reverse.map( x => Reg( init = UInt( mem(x), 1 ) ) ) )

  // connect the registers for the shift
  io.dOut := UInt(0)
  when( io.vld ){
    sr(0) := io.dIn
    for (idx <- 0 until len-1){ sr(idx+1) := sr(idx) }
    io.dOut := sr(len-1)
  }
}

class SRL16E_IO extends Bundle {
  val dIn = Bool( INPUT )
  val vld = Bool( INPUT )
  val a0 = Bool( INPUT) //UInt( INPUT, width=4 )
  val a1 = Bool( INPUT)
  val a2 = Bool( INPUT)
  val a3 = Bool( INPUT)
  val dOut = Bool( OUTPUT )

  def setNames(){
    dIn.setName("D")
    vld.setName("CE")
    a0.setName("A0")
    a1.setName("A1")
    a2.setName("A2")
    a3.setName("A3")
    dOut.setName("Q")
  }

}

class SRL16Ew( val hex : String, val len : Int ) extends BlackBox {
  
  val io = new SRL16E_IO
  io.setNames()

  renameClock(Driver.implicitClock, "CLK")

  setVerilogParameters(
    "#( .INIT(32'h" + hex + ") )\n"
    )
  setModuleName("SRL16E")

  // Test the module
  val sreg = Module( new SRL16ETester( hex, len ) )
  sreg.io.dIn := io.dIn
  sreg.io.vld := io.vld
  io.dOut := sreg.io.dOut
  
  
}

class SRL16E( val hex : String, val len : Int ) extends Module{
  val io = new Bundle{
    val dIn = Bool( INPUT )
    val vld = Bool( INPUT )
    val len = UInt( INPUT, width=4 )
    val dOut = Bool( OUTPUT )
  }

  // instantiate module, make default connections
  val srl16e = Module( new SRL16Ew( hex, len ) )
  srl16e.io.dIn := io.dIn
  srl16e.io.vld := io.vld
  srl16e.io.a0 := io.len(0)
  srl16e.io.a1 := io.len(1)
  srl16e.io.a2 := io.len(2)
  srl16e.io.a3 := io.len(3)
  io.dOut := srl16e.io.dOut

}


/*
To generate the verilog
*/

class SRL16E_inst extends Module {
  val io = new Bundle {
    val a = Bool( INPUT )
    val b = Bool( OUTPUT )
  }

  val len = 16
  val srl = Module( new SRL16E( "0000", len ) )
  srl.io.dIn := io.a
  srl.io.len := UInt(len-1, width=4)
  srl.io.vld := Bool(true)
  io.b := srl.io.dOut

}

// generate verilog
object SRL16EVerilog {


  def main(args: Array[String]): Unit = {
    println("Generating verilog for fastfood module")
    chiselMain(Array("--backend", "v", "--targetDir", "verilog"), 
                () => Module( new SRLC32E_inst ) ) 

  }
}
