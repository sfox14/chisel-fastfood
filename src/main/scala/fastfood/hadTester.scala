package fastfood

import Chisel._
import scala.util.Random
import com.github.tototoshi.csv._
import utils._

// generate verilog
object hadVerilog {

  val n_dicts = 16
  val n_features = 8
  val n_stack = 2
  val bitWidth = 18
  val fracWidth = 10

  // includes padding 

  def main(args: Array[String]): Unit = {
    println("Generating verilog for the Hadamard module")
    chiselMain(Array("--backend", "v", "--targetDir", "verilog"), 
                () => Module( new HAD( n_features, bitWidth, fracWidth) ) )
  }
}


// create a basic simulation
class hadSim( c: HAD, verbose : Boolean = true ) extends Tester(c) {

  val rng = new Random( 23 )

  val n = c.n_features
  //val X = random_data( 10, n, c.fracWidth, rng )

  val x = List(1.67,0,0,1.87,-2.3,-1,0,0.86).map(x => toFixed(x, c.fracWidth) )
  val X = List(x)

  for (ix <- 0 until X.length){

    for( iy <- 0 until n){ poke( c.io.dIn(iy), X(ix)(iy) ) }

    for (iz <- 0 until 5){
      step(1)
      //for( iy <- 0 until n){ peek( c.io.dOut(iy)  ) }
      for( iy <- 0 until n){ 
        println( fromPeek.toDbl( peek( c.io.dOut(iy) ), 
                              c.bitWidth, c.fracWidth) )
      }
    }

  } 
  
}

object hadTester {

  //val n_dicts = 8
  val n_features = 8
  //val n_stack = 1
  val bitWidth = 18
  val fracWidth = 10

  val z = List(1, 2, 6)

  def main(args: Array[String]): Unit = {
    println("Testing the Hadamard module")
    
    chiselMainTest(Array("--genHarness", "--test", "--backend", "c", //"--wio", 
      "--compile", "--targetDir", ".emulator"), // .emulator is a hidden directory
      () => Module(new HAD( n_features, bitWidth, fracWidth, z ) ) ) {
        f => new hadSim(f, true)
      }
  }

}
