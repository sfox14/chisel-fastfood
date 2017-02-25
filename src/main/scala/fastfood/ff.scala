package fastfood

import Chisel._
import com.github.tototoshi.csv._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.util.Random

import utils._

/*
Fastfood Implementation
-----------------------
1. B = {-1,+1}
2. G = {-1, +1} or N(0,1)
*/
class FF( val n : Int, val d : Int, val k : Int, val bitWidth : Int, val fracWidth : Int,
             val SR : List[List[Int]] ) extends Module {

  /*
  n - number of support vectors
  d - input dimensionality
  k - 

  */

  val io = new Bundle{

    val dIn = Vec.fill( k ){ Fixed( INPUT, bitWidth, fracWidth ) }
    val inValid = Bool(INPUT)
    val dOut = Fixed( OUTPUT, bitWidth, fracWidth )
    val outValid = Bool(OUTPUT)

  }

  /*
  Some scala stuff here to generate BHP or BHPGH:
  rng = random.seed(42)
  returns kxd elems of 1,0 (+1,-1)
  */

  // layer 1
  val gphbx = Module( new GPHBx(d, k, bitWidth, fracWidth, SR(0), BigInt(1), 
                      adderType.binAdd2, count.log2Up, out.direct ) )

  gphbx.io.dIn.bits := io.dIn
  gphbx.io.dIn.valid := io.inValid
  io.dOut := gphbx.io.dOut.bits
  io.outValid := gphbx.io.dOut.valid
  
  
}


