package parallel

import Chisel._
import scala.util.Random
import com.github.tototoshi.csv._
import utils._

import math._

// generate verilog
object outVerilog {

  val n_dicts = 16
  val bitWidth = 18
  val fracWidth = 10

  // includes padding 
  def main(args: Array[String]): Unit = {
    println("Generating verilog for the readout module")
    chiselMain(Array("--backend", "v", "--targetDir", "verilog"), 
                () => Module( new OUT( n_dicts, bitWidth, fracWidth ) ) )
  }
}

// create a basic simulation
class outSim( c: OUT, verbose : Boolean = true ) extends Tester(c) {

  val rng = new Random( 23 )

  val N = 10
  val n = c.n_dicts
  val stages = c.n_cycles
  println( stages )

  val alpha = (0 until n).map( x => rng.nextDouble ).toList
  val alpha_fixed = alpha.map(a => toFixed(a, c.fracWidth))


  // generate some data
  val x_raw = (0 until N ).map( x => ( 0 until n ).map( y => rng.nextDouble ) )
  val X = x_raw.map( x => x.map( y => toFixed(y, c.fracWidth) ) )
  //val Y = X.map( x => (ram, x).zipped.map(_*_).reduce(_+_) )
  val y = x_raw.map( x => (alpha, x).zipped.map(_*_).reduce(_+_) )
  val Y = X.map( x => (alpha_fixed, x).zipped.map(_*_)
            .map(x => x >> c.fracWidth).reduce(_+_) )


  val valid = Range(stages, stages+N, 1).toList
  println( valid )

  var cx = 0
  for (ix <- 0 until X.length){

    for( iy <- 0 until n){ 
      poke( c.io.dIn(iy), X(ix)(iy) )
      poke( c.io.alpha(iy), alpha_fixed(iy) ) 
    }

    if ( ix == valid(cx) ){
      println( "valid" )
      expect( c.io.dOut, Y(cx) )
      println( fromPeek.toDbl( peek( c.io.dOut ), 
                              c.bitWidth, c.fracWidth) )
      println( y(cx) )
      cx +=1
    }

    step(1)
    
  }
  
  // get remaining examples
  while (cx < N) {
    println( "valid" )
    expect( c.io.dOut, Y(cx) )
    println( fromPeek.toDbl( peek( c.io.dOut ), 
                            c.bitWidth, c.fracWidth) )
    println( y(cx) )
    cx +=1

    step(1)
  }

     
  
}

object outTester {

  val n_dicts = 4
  val bitWidth = 18
  val fracWidth = 10


  def main(args: Array[String]): Unit = {
    println("Testing the readout module")
    
    chiselMainTest(Array("--genHarness", "--test", "--backend", "c", 
      "--compile", "--targetDir", ".emulator"),
      () => Module(new OUT( n_dicts, bitWidth, fracWidth ) ) ) {
        f => new outSim(f, true)
      }
  }

}
