package parallel

import Chisel._
import utils._
import xilinx._


/*
GPHBx layer - Serial Processor ( k features per cycle )
            - Code for one dot product. there are n of these modules in fastfood
*/
class GPHBx( val n_features : Int, val n_paths : Int, val bitWidth : Int, val fracWidth : Int,
  val ram : List[Int], val G : BigInt, adderFunc : Vector[Fixed] => Fixed, 
  cycleFunc : Int => Int, outFunc : (Fixed, Fixed, Bool) => (Fixed, Bool), 
  forSim : Boolean = true ) extends Module {

  val io = new Bundle{

    val dIn = Decoupled( Vec.fill( n_paths ){ Fixed( INPUT, bitWidth, fracWidth ) } ).flip
    val dOut = Decoupled( Fixed( OUTPUT, bitWidth, fracWidth ) ) 

  }
  /*
  Cycle Parameters:
  	iCycles - number of dIn vecs per feature
  	aCycles - number of stages in the adder tree (triAdders would be different)
  	nCycles - latency of module. ( = add + 1 cycle for acc + remaining dIn vecs )
  */
  val iCycles = math.ceil( n_features/n_paths.toFloat ).toInt 
  val aCycles = cycleFunc( n_paths ) // for adder tree
  val nCycles = (aCycles + 1) + (iCycles - 1) 

  /*
  Select the G matrix:
    1.  binary    -   Hadamard or G = {-1,1}
    2.  ternary   -   G = {-1,0,1} 

  Instantiates the LUT-based Shift Register using SRLC32E primitive. Connects the
  dIn.valid to the clock enable. The output is used as select lines for the
  {-1,0,+1} Muxes.

  We never need to use ternary for fastfood because GPHB simplifies to binary
  except some rows are fully zero. This reduces the number of dot product
  computations required, thus eliminating gphbx modules       
  */

  val vdIn = stream.binary( io.dIn, n_paths, ram, forSim )
  //val vdIn = stream.ternary( io.dIn, n_paths, ram, forSim)


  // two, three or four input adds
  val accReg = RegInit( Fixed(0, bitWidth, fracWidth) )
  val adderValid = RegInit( Bool(false) )
  if ( aCycles == 1 ){
  	adderValid := io.dIn.valid
  	//println("aCycles = 1")
  } else{
  	adderValid := ShiftRegister( io.dIn.valid, aCycles-1 )
  	//println("aCycles > 1")
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

  // apply the out layer (either a mul or direct connect)
  val (res, vld) = outFunc( accReg, Fixed( G, bitWidth, fracWidth), outValid )
  
  io.dOut.bits := res   //accReg
  io.dOut.valid := vld //outValid

}

