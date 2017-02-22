package fastfood

import Chisel._
import utils._
import xilinx._

/*
GPHBx layer - Serial Processor ( t features per cycle )
*/
class GPHBx( val d : Int, val k : Int, val SR : List[Int], adderFunc : Vector[Fixed] => Fixed, 
					val bitWidth : Int, val fracWidth : Int, forSim : Boolean = true ) extends Module {

  val io = new Bundle{

    val dIn = Decoupled( Vec.fill(k){ Fixed( INPUT, bitWidth, fracWidth ) } ).flip
    val dOut = Decoupled( Fixed( OUTPUT, bitWidth, fracWidth ) ) 

  }
  /*
  Cycle Parameters:
  	iCycles - number of dIn vecs per feature
  	aCycles - number of stages in the adder tree (triAdders would be different)
  	nCycles - latency of module. ( = add + 1 cycle for acc + remaining dIn vecs )
  */
  val iCycles = math.ceil(d/k.toFloat).toInt 
  val aCycles = log2Up( k ) // for binAdder
  val nCycles = (aCycles + 1) + (iCycles - 1) 

  println(iCycles)
  println(aCycles)
  println(nCycles)

  // get the hex number. given as list from LSB to MSB, therefore reverse and convert, must be 32-bit hex string 
  val hex = BigInt( SR.reverse.map(x=>x.toString).mkString, 2 ).toString(16).toList.reverse.padTo(8,0).reverse.map(x=>x.toString).mkString 
  println( hex )

  // Multiply GPHB, i.e. {-1, 0, 1}
  val num_sel = k //if ternary then num_sel = 2*k
  val sreg = Module( new LUTSR32( Vec.fill( k ){ Bool() }, hex, num_sel, d, forSim ) )
  sreg.io.vld := io.dIn.valid
  val vdIn = (0 until k).map(x => Mux( sreg.io.out(x), io.dIn.bits(x), -io.dIn.bits(x) ) ).toVector



  // two, three or four input adds
  val accReg = RegInit( Fixed(0, bitWidth, fracWidth) )
  val adderValid = RegInit( Bool(false) )
  if ( aCycles == 1 ){
  	adderValid := io.dIn.valid
  	println("aCycles = 1")
  } else{
  	adderValid := ShiftRegister( io.dIn.valid, aCycles-1 )
  	println("aCycles > 1")
  }

  /*
  counter 	- incremented when adderValid == true, i.e. a valid adder tree output
			- counts up to iCycles ( == number of dIn vecs making up a feature)
			- therefore, width = iCycles+1
  */
  val counter = RegInit( UInt(0, width = log2Up( (nCycles - aCycles) + 1 )) )
  
  /*
  outValid 	- the output is valid when the counter == iCycles. This is because after the
  			  adder tree, the result is pipelined (i.e. 1 x accReg). So it takes as many
  			  increments as there are dIn vecs to be accumulated.
  			- outValid == true, i.e. all dIn vecs of a given feature have been accumulated 
  */
  val outValid = ( counter === UInt( nCycles - aCycles ) )

  // adderFunc ecpects a Vector[Fixed]
  //val vdIn = io.dIn.bits.toVector

  // normal assignments
  when( adderValid ){
  	counter := counter + UInt(1)
  	accReg := accReg + adderFunc( vdIn )
  }

  // override assignments when outValid
  when( outValid ){
  	when( adderValid ){
  		/*
		There is a VALID adder result for the next feature. We should set the counter == 1
		and the accReg == adderFunc result.
  		*/
  		counter := UInt( (nCycles - aCycles ) - (iCycles - 1) )
  		accReg := adderFunc( vdIn )
  	}.otherwise{
  		/*
		The next adder result is INVALID. We should reset the counter == 0 and the accReg == 0.
  		*/
  		counter := UInt( 0 )
  		accReg := Fixed(0, bitWidth, fracWidth)
  	}
  }

  io.dOut.bits := accReg
  io.dOut.valid := outValid

}

