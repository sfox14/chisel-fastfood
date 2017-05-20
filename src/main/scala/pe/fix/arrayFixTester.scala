package fix

import Chisel._
import com.github.tototoshi.csv._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.util.Random

// import breeze library stuff: https://github.com/scalanlp/breeze/wiki
import breeze.stats.distributions._
import breeze.linalg._
import breeze.numerics._
import breeze.math._

import scala.collection.immutable.Vector //to override breeze.Vector

import utils._

//import math._



class arrayFixSim( c: ARRAYfix ) extends Tester( c ){

  val n = c.n
  val d = c.d
  val p = c.p
  val fracWidth = c.fracWidth
  val bitWidth = c.bitWidth

  var k = 0

  val (xdat, ydat) = helper.genData(d, 10, 2, 10000, 20)
  println( xdat.length )
  println( xdat(0).length ) 
  println( ydat.length )

  val rng = new Random( 33 )

  poke( c.io.inData.valid, true )

  // just the first 4 training examples
  for( ix <- 0 until 4 ){
    poke( c.io.inData.bits(1), toFixed( ydat(ix), fracWidth ) )
    for( iy <- 0 until d ){
      poke(c.io.inData.bits(0), toFixed( xdat(ix)(iy), fracWidth ) )
      step(1)
    }
  }
  step(450)

  /*
  for( ix <- 0 until d*2 ){
    poke( c.io.inData.bits(0), toFixed( rng.nextFloat, fracWidth ) )
    step(1)
    peek( c.io.outData.bits )
  }
  step(450)
  */

}

object helper{

  def fit(n : Int, d : Int, sigma : Double) : (Vector[Double], Vector[Double], Vector[Double]) = {

    implicit val basis : RandBasis = RandBasis.withSeed(0)
    val rng1 = new Gaussian(0, 1)
    val rng2 = new ChiSquared( d )

    // original gmat and smat
    var gmat = DenseMatrix.rand( n/d, d, rng1 )
    var smat = DenseMatrix.rand( n/d, d, rng2 )

    //calculate the frobenius norm of G, (||G||_frob)^1/2
    //val frob = sqrt( sum( (g *:* g).copy(*, ::) ) ) // multiply elementwise, then sum across each row
    val frob = norm( gmat(*, ::) ) // frobenius norm of each row   
    smat = smat(::, *) *:* (1.0 /:/ frob)
    smat = ( 1.0/( sigma*sqrt(d) ) ) *:* smat

    //get equivalent n-length g,s,alpha vectors
    ( gmat.t.toDenseVector.toArray.toVector,
      smat.t.toDenseVector.toArray.toVector,
      rng1.sample( n ).toVector )

  }

  def genData( d : Int = 8, lookAhead : Int = 10, step : Int = 2, 
      n_samples : Int = 10000, n_rate : Int = 20 ) : (Array[Array[Double]], Array[Double]) = {
    // generate a basic dataset for modeling a Sine Wave
    //    # d - number of features
    //    # lookAhead - how far ahead to predict
    //    # step - sliding window step
    //    # n_samples - size of raw sine data
    //    # n_rate - how many samples per radian, pi*radians in a sine period

    val xraw = linspace(0, n_samples/n_rate, n_samples)
    val xsine = sin(xraw).toArray
    val ydat = xsine.sliding(d+lookAhead, step).toArray.map( x => x.last ) 
 
    ( xsine.sliding(d, step).toArray.take( ydat.length ), ydat)

  }

}


object arrayFixTester{

  val bitWidth = 18
  val fracWidth = 10

  val sigma = 11.47
  val n = 16 //16  //16
  val p = 4 //4   //2
  val d = 8

  // alu stages
  val aStages = 4

  // get n-length parameter vectors
  val (gvec, svec, avec) = helper.fit( n, d, sigma )
  
  // convert to fixed point, and group to PEs
  val g = gvec.map(x => toFixed(x, fracWidth) ).grouped( n/p ).toVector
  val s = svec.map(x => toFixed(x, fracWidth) ).grouped( n/p ).toVector
  val alpha = avec.map(x => toFixed(x, fracWidth) ).grouped( n/p ).toVector

  println( g )
  println( s )
  println( alpha )


  def main(args: Array[String]): Unit = {
    println("Testing the FSM for controlling the array")
    
    chiselMainTest(Array("--genHarness", "--test", "--backend", "c", //"--wio", 
      "--compile", "--targetDir", ".emulator", "--vcd"), // .emulator is a hidden directory
      () => Module(new ARRAYfix( bitWidth, fracWidth, n, p, d, g, s, 
                        alpha, aStages, false, true ) ) ) {
        f => new arrayFixSim( f )
      }
  }
}

//not working for d==p, and also if aStages<3 and cosFlag=true, and this is because the
// alu stages is less than mStages.