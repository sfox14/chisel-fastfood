package fastfood

import Chisel._
import scala.util.Random
import com.github.tototoshi.csv._
import utils._

import math._

// generate verilog
object lutVerilog {

  val bitWidth = 18
  val fracWidth = 10

  val s = toFixed( 2.35, fracWidth )

  // includes padding 
  def main(args: Array[String]): Unit = {
    println("Generating verilog for the Cosine Look-Up module")
    chiselMain(Array("--backend", "v", "--targetDir", "verilog"), 
                () => Module( new LUT( bitWidth, fracWidth, s ) ) )
  }
}

// create a basic simulation
class lutSim( c: LUT, verbose : Boolean = true ) extends Tester(c) {

  val rng = new Random( 23 )

  val N = 10

  // generate some data
  val x_raw = (0 until N ).map( x => rng.nextDouble )
  val X = x_raw.map( x => toFixed(x, c.fracWidth)  )
  //val Y = X.map( x => (ram, x).zipped.map(_*_).reduce(_+_) )

  println(X)

  for (ix <- 0 until X.length){

    poke( c.io.dIn, X(ix) )

    for (iz <- 0 until 5){
      step(1)      
      println( fromPeek.toDbl( peek( c.io.dOut ), 
                              c.bitWidth, c.fracWidth) )
      println( cos( x_raw(ix)*Pi ) )

    }

  } 
  
}

object lutTester {

  val bitWidth = 18
  val fracWidth = 10

  val s = toFixed( 2.35, fracWidth )

  def main(args: Array[String]): Unit = {
    println("Testing the Cosine Look-Up table module")
    
    chiselMainTest(Array("--genHarness", "--test", "--backend", "c", //"--wio", 
      "--compile", "--targetDir", ".emulator"), // .emulator is a hidden directory
      () => Module(new LUT( bitWidth, fracWidth, s ) ) ) {
        f => new lutSim(f, true)
      }
  }

}
