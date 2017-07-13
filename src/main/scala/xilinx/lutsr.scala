package xilinx

import Chisel._


/*
Static lut-based shift register with variable width and len (depth)
  - builds variable width sreg from multiple 1-bit SRL32E primitives 
  - t = number of parallel data paths 
*/

class LUTSR32[ T <: Data ] ( init : List[Int], t : Int ) extends Module {
  
  val io = new Bundle{
  	val vld = Bool(INPUT)
  	val out = Vec.fill( t ){ Bool(OUTPUT) }
  }

  /*
  Takes a sequence of binary Ints, and converts them to hex strings:
    - List[Int] => List[ List[String] ]
  */

  val tli = (0 to t-1).map(x => init.drop(x).grouped( t )
              .map(_.head).toList.grouped(32)
              .map(y => y.reverse ).toList )// creates t lists, each of which is a list with 32 binary ints 
  val tbs = tli.toList.map(x => x.map(y => y.map(z => z.toString).mkString) ) // convert to lists of binary strings
  val ths = tbs.map(x => x.map(y => BigInt(y, 2)
            .toString(16).toList
            .reverse.padTo(8,0)
            .reverse.map(x=>x.toString).mkString ) )
  val tA = tli.toList(0).map(x => x.length) // the len fo rach SRLC32E

  val num_layers = math.ceil( init.length/(t*32).toDouble ).toInt
  Predef.assert( num_layers == tA.length, "Error: Number of layers do not match hex string")
  
  /*
  Build the architecture:
    - build using SRLC32E Xilinx primitives or,
    - chisel shift register 
  */
  val w = 5
  //val arch = (0 to num_layers-1).map(x => (0 to t-1).map(y => Module( new SRLC32E( ths(y)(x), tA(x), false ) ) ) )
  val arch = (0 to num_layers-1).map(x => (0 to t-1).map(y => Module( new ShiftRegInit( ths(y)(x), tA(x) ) ) ) )
  
  /*
  Make default connections:
  */
  if (num_layers == 1){
  	// easiest option - arch.last == arch(0)
  	val mod = arch(0)
  	val len = tA(0)
  	for (ix <- 0 until t){
  		// for each path setup default connections
  		mod(ix).io.dIn := mod(ix).io.dCas
  		mod(ix).io.vld := io.vld
  		mod(ix).io.len := UInt(len-1, w) //sets depth of sreg
  		io.out(ix) := mod(ix).io.dOut
  	}
  } else if (num_layers == 2){
  	val mod2 = arch(1)
  	val mod1 = arch(0)
  	val len = tA(1)

  	for (ix <- 0 until t){
  		mod2(ix).io.dIn := mod1(ix).io.dCas
  		mod1(ix).io.dIn := mod2(ix).io.dCas
  		mod2(ix).io.len := UInt(len-1, 5)
  		mod1(ix).io.len := UInt(31, 5)
  		mod2(ix).io.vld := io.vld
  		mod1(ix).io.vld := io.vld
  		io.out(ix) := mod1(ix).io.dOut
  	}

  } else {
    // more than two 32 bit stages required (large input feature space)
    // connect all layers except the last one
    val len = tA.last
    for (ix <- 0 until num_layers-1){
      for (iy <- 0 until t){
        arch(ix)(iy).io.dIn := arch(ix+1)(iy).io.dCas
        arch(ix)(iy).io.len := UInt(31, width=5)
        arch(ix)(iy).io.vld := io.vld
      }
    }
    // connect final layer, and bottom layer output
    for (ix <- 0 until t){
      arch.last(ix).io.dIn := arch(0)(ix).io.dCas
      arch.last(ix).io.len := UInt(len-1, 5)
      arch.last(ix).io.vld := io.vld
      io.out(ix) := arch(0)(ix).io.dOut
    }
  } 

}


/*
Top module for testing LUTSR32
*/
class LUTSR( val init : List[Int], val t : Int ) extends Module {
	
	val io = new Bundle{
		val vld = Bool(INPUT)
		val out = Vec.fill(t){ Bool(OUTPUT) }
	}

	val sreg = Module( new LUTSR32( init, t )  )

	sreg.io.vld := io.vld
	io.out := sreg.io.out
}


/*
Generate verilog
*/
object LutSRVerilog {

  val hex = "f0f0f0f0"
  val t = 3
  val d = 32

  // converts hex to list of ints (MSB to LSB)
  val ram = ( ( BigInt( hex, 16 ).toString(2) ) map(_.asDigit) ).toList.reverse.padTo(d, 0).reverse

  def main(args: Array[String]): Unit = {
    println("Generating verilog LUT-based Shift Register (Synthesis with Xilinx Tools)")
    chiselMain(Array("--backend", "v", "--targetDir", "verilog"), 
                () => Module( new LUTSR( ram, t ) ) ) 

  }
}

/*
Simulation for LUTSR
*/
class LutSRSim( c: LUTSR ) extends Tester(c) {

	val init = c.init
	val paths = c.t


	val bin = init
	var k = 0

  println( bin, bin.length )

	// init sreg. should enq but not deq
  println("\n Round 1: \n")
	poke( c.io.vld, true )
	for (ix <- 0 until (bin.length/paths.toFloat).toInt){
    for (t <- 0 until paths ){
      expect( c.io.out(t), bin(k+t) )
    }
		step(1)
    k = k + paths
    println(k)
	}

  // if not divisible by the number of paths then peek remainder
  if ( bin.length%paths != 0 ){
    peek( c.io.out )
    step(1)
  }

  println("\n Invalid: \n")
  poke( c.io.vld, false )
  peek( c.io.out )
  step(1)
  peek( c.io.out )
  step(1)
  println("\n Round 2: \n")
  
  k = 0
  poke( c.io.vld, true )
  for (ix <- 0 until (bin.length/paths.toFloat).toInt){
    for (t <- 0 until paths ){
      expect( c.io.out(t), bin(k+t) )
    }
    step(1)
    k = k + paths
  }

}

/*
LUTSR Tester
*/
object LutSRTester{

  val hex = "0f0f005"
  val t = 3  // number of parallel data paths
  val d = 26

  // converts hex to list of ints (LSB to MSB)
  val ram = ( ( BigInt( hex, 16 ).toString(2) ) map(_.asDigit) ).toList.reverse.padTo(d, 0)

  println( ram )

	def main(args: Array[String]): Unit = {
	println("Testing the LUT-based shift register")

	chiselMainTest(Array("--genHarness", "--test", "--backend", "c", 
	  "--compile", "--targetDir", ".emulator"),
	  () => Module(new LUTSR( ram, t ) ) ) {
	    f => new LutSRSim(f)
	  }
	}

}


















