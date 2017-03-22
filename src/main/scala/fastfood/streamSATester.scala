package fastfood

import Chisel._
import scala.util.Random
import com.github.tototoshi.csv._
import utils._

import scala.collection.mutable.Buffer 

// generate verilog
object ssaVerilog {

  val n_dicts = 600
  val n_features = 100
  val bitWidth = 18
  val fracWidth = 10

  val s = 1
  val p = 600
  val col = 1
  val a = 1


  val rng = new Random( 23 )
  val ram = (0 until n_dicts).map( x => ( 0 until n_features ).map( y => 
                        rng.nextInt(2) ).toList ).toList


  def main(args: Array[String]): Unit = {
    println("Generating verilog for stream systolic array dot product")
    chiselMain(Array("--backend", "v", "--targetDir", "verilog"), 
                () => Module( new StreamSA( n_dicts, n_features, bitWidth, fracWidth, 
                                        ram, a, p, col, s ) ) )
  }
}


// create a basic simulation
class ssaSim( c: StreamTop, verbose : Boolean = true ) extends Tester(c) {

  val rng = new Random( 31 )

  val n_features = c.n_features
  val n_dicts = c.n_dicts

  val X = (0 until 2)
    .map( x => (0 until n_features)
    .map( y => toFixed( rng.nextDouble*0.1, c.fracWidth ) ) )

  val Y = X.map( x=> BigInt(x.reduce(_+_).toString, 10).toString(16) )

  println(X)
  println(Y)

  // apply a reset  
  reset(20)
  var iy = 0
  var done = false
  while( iy < 2){

    for ( ix <- 0 until n_features ){

      var en = (rng.nextInt(5) >= 2)
      poke( c.io.dIn.valid, en )
      while( !en ){
        poke( c.io.dIn.bits, toFixed( rng.nextDouble, c.fracWidth ) )
        step(1)
        en = (rng.nextInt(5) >= 2)
        poke( c.io.dIn.valid, en)
      }

      poke( c.io.dIn.bits, X(iy)(ix) )

      peek( c.io.dOut.valid )
      peek(c.io.dOut.bits)
      step(1)
    }
    iy +=1
    
  }

  poke( c.io.dIn.valid, false )
  for( ix <- 0 until 50){
    step(1)
  }  

  
}

object ssaTester {

  val n_dicts = 12
  val n_features = 8
  val bitWidth = 18
  val fracWidth = 10

  val s = 1
  val p = 6
  val col = 1
  val a = 1


  val rng = new Random( 23 )
  val ram = (0 until n_dicts).map( x => ( 0 until n_features ).map( y => 
                        rng.nextInt(2) ).toList ).toList



  def main(args: Array[String]): Unit = {
    println("Testing the stream systolic array dot product")
    
    chiselMainTest(Array("--genHarness", "--test", "--backend", "c", //"--wio", 
      "--compile", "--targetDir", ".emulator", "--vcd"), // .emulator is a hidden directory
      () => Module(new StreamTop( n_dicts, n_features, bitWidth, fracWidth, ram, 
                                  a, p, col, s ) ) ) {
        f => new ssaSim(f, true)
      }
  }

}
