package fastfood

import Chisel._
import utils._
import xilinx._

import math._

import scala.collection.mutable.Buffer


/*
S and LUT layer   - Parallel Processor
                  - Multiplies with S
                  - Implements a COSINE look up table using a BRAM
                  - *TO DO: try implement COSINE using linear interpolation*
*/
class LUT( val bitWidth : Int, val fracWidth : Int, val S : BigInt, 
                lutSize : Int = 512, A : Double = 1.0, b : Double = 0.0) extends Module {

  val io = new Bundle{

    val dIn = Fixed( INPUT, bitWidth, fracWidth ) 
    val dOut = Fixed( OUTPUT, bitWidth, fracWidth ) 

  }

  // 512 x bitWidth ROMs for Cosine LUT
  def cosTable(A : Double, b: Double, n : Int = 512) = {

    val tab = (0 until n).map(x => x/(1<<(log2Up(n)-1) ).toDouble )
              .map(y => A*cos(y*Pi + b) )
    val fixedTab = tab.map( x => Fixed(toFixed(x, fracWidth), bitWidth, fracWidth) )

    Vec(fixedTab)
  }


  val intWidth = (bitWidth - fracWidth)
  val lutWidth = log2Up( lutSize ) // 2**(lutWidth) = lutSize entries
  val lutFracWidth = lutWidth - 1 // only one bit for integer (mod 2)
  
  Predef.assert( (lutSize & (lutSize - 1)) == 0, "Error: Cosine LUT is not a power of 2")
  Predef.assert( fracWidth>lutFracWidth, "Error: Not enough bits for Cosine LUT")

  // for fracWidth
  val cosWave = cosTable(A, b, lutSize)
  val index = io.dIn( (bitWidth - intWidth) , (fracWidth - lutFracWidth) )

  println( lutWidth )
  println( index.getWidth )
  Predef.assert( index.getWidth == lutWidth, "Error: Index width does not match Cosine LUT " )

  io.dOut := cosWave( index )

}
