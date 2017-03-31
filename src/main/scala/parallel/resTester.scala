package parallel

import Chisel._
import scala.util.Random
import com.github.tototoshi.csv._
import utils._

// generate verilog
object resVerilog {

  val n_dicts = 16
  val bitWidth = 18
  val fracWidth = 10

  // includes padding 

  def main(args: Array[String]): Unit = {
    println("Generating verilog for the Reservoir module")
    chiselMain(Array("--backend", "v", "--targetDir", "verilog"), 
                () => Module( new RES( n_dicts, bitWidth, fracWidth ) ) )
  }
}

// create a basic simulation
class resSim( c: RES, verbose : Boolean = true ) extends Tester(c) {

  val rng = new Random( 23 )

  val n = c.n_dicts
  val ram = c.ram

  // generate some data
  val X = random_data( 1, n, c.fracWidth, rng )
  val Y = X.map( x => (ram, x).zipped.map(_*_).reduce(_+_) )

  println(X)
  println(ram)
  println(Y)


  for (ix <- 0 until X.length){

    for( iy <- 0 until n){ poke( c.io.dIn(iy), X(ix)(iy) ) }

    for (iz <- 0 until 5){
      step(1)      
      println( fromPeek.toDbl( peek( c.io.dOut ), 
                              c.bitWidth, c.fracWidth) )
      
    }

  } 
  
}

object resTester {

  val n_dicts = 8
  val bitWidth = 18
  val fracWidth = 10

  val r = List(-1,0,0,-1,-1,-1,-1,1)

  def main(args: Array[String]): Unit = {
    println("Testing the Reservoir module")
    
    chiselMainTest(Array("--genHarness", "--test", "--backend", "c",
      "--compile", "--targetDir", ".emulator"), // .emulator is a hidden directory
      () => Module(new RES( n_dicts, bitWidth, fracWidth, r ) ) ) {
        f => new resSim(f, true)
      }
  }

}
