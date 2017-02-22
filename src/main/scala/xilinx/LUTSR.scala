package xilinx

import Chisel._



// static ram-based shift register with variable width and len (depth)
class LUTSR32[ T <: Data ] ( genType : T, hex : String, t : Int, d : Int, forSim : Boolean = true ) extends Module {
  
  // d - the length of the hex string with valid data (* else - we lose zeros )
  // t - number of parallel sreg's. # of data paths multiplied by number of bits required (for {-1,0,1} need 2 bits for each path)

  val io = new Bundle{
  	val vld = Bool(INPUT)
  	val out = Vec.fill( t ){ Bool(OUTPUT) }
  }

  /*
  Some Scala code which takes the init hex string and converts it to "t"
  32-bit hex strings, for each serial datapath.
  */

  val bs = BigInt( hex, 16 ).toString(2) // hex string to binary string
  val bi = ( bs map(_.asDigit) ).toList.reverse.padTo(d, 0).reverse // binary string to List of bin ints
  val tli = (0 to t-1).map(x => bi.reverse.drop(x).grouped( t ).map(_.head).toList.grouped(32).map(y => y.reverse ).toList )// creates a Vector of t Lists 
  val tbs = tli.toList.map(x => x.map(y => y.map(z => z.toString).mkString) ) // converts the t Lists to t binary strings
  val ths = tbs.map(x => x.map(y => BigInt(y, 2).toString(16).toList.reverse.padTo(8,0).reverse.map(x=>x.toString).mkString ) )
  val tA = tli.toList(0).map(x => x.length) // the number of len for each SRLC32E, the first length of each layer in first channel

  val num_layers = math.ceil( bi.length/(t*32).toDouble ).toInt
  Predef.assert( num_layers == tA.length, "Error: Number of layers do not match hex string")

  println(bi)
  println(tli)
  println(tbs)
  println(ths)
  println(tA)

  /*
  Build the architecture:
  */
  val w = 5
  val arch = (0 to num_layers-1).map(x => (0 to t-1).map(y => Module( new SRLC32E( ths(y)(x), tA(x), forSim ) ) ) )
  //if ( num_layers == 1) {
  //val w = 4
  //val arch = (0 to num_layers-1).map(x => (0 to t-1).map(y => Module( new SRL16E( ths(y)(x), tA(x) ) ) ) )
  //}
  
  println(arch)
  
  /*
  Make default connections:
  */
  println( s"Number of layers: $num_layers" )
  println( s"Number of paths: $t" )
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



class LUTSR( val hex : String, val t : Int, val d : Int, forSim : Boolean = true) extends Module {
	
	val io = new Bundle{
		val vld = Bool(INPUT)
		val out = Vec.fill(t){ Bool(OUTPUT) }
	}

	val sreg = Module( new LUTSR32( Vec.fill(t){ Bool() }, hex, t, d, forSim )  )

	sreg.io.vld := io.vld
	io.out := sreg.io.out
}


// generate verilog
object LutSRVerilog {

  //val hex = "ff00ff0000000fffffff000000000fffff00ff0000000fffffff000000000ffff"
  val hex = "f0f0f0f0"
  val t = 3
  val d = 32

  def main(args: Array[String]): Unit = {
    println("Generating verilog LUT-based Shift Register (Synthesis with Xilinx Tools)")
    chiselMain(Array("--backend", "v", "--targetDir", "verilog"), 
                () => Module( new LUTSR( hex, t, d, false ) ) ) 

  }
}


class LutSRSim( c: LUTSR ) extends Tester(c) {

	val init = c.hex
	val paths = c.t


	val bin = ( BigInt( init, 16 ).toString(2) map(_.asDigit) ).toList.reverse
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


object LutSRTester{

	//val hex = "ff00ff0000000fffffff000000000fffff00ff0000000fffffff000000000ffff" // the hex string (MSB to LSB)
	//val hex = "f0f0f0f0"
  val hex = "f0f0"
  val t = 3  // number of parallel data paths
  val d = 16

	def main(args: Array[String]): Unit = {
	println("Testing the LUT-based shift register")

	chiselMainTest(Array("--genHarness", "--test", "--backend", "c", //"--wio", 
	  "--compile", "--targetDir", ".emulator"), // .emulator is a hidden directory
	  () => Module(new LUTSR( hex, t, d, true ) ) ) {
	    f => new LutSRSim(f)
	  }
	}

}


















