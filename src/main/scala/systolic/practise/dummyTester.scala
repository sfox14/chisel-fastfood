package practise

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
import com.github.tototoshi.csv._



object helper{

  def fit(n : Int, d : Int, sigma : Double) : 
  (Vector[Double], Vector[Double], Vector[Double]) = {

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

object dummyVerilog{

  val bitWidth = 18
  val fracWidth = 9
  
  val sigma = 11.47
  val n = 512
  val p = 128
  val d = 256

  val k = n/p
  val h = n/d 
  val b = p/h // 2

  println(s"Dictionary Size: $n")
  println(s"Number of PEs: $p")
  println(s"Number of Features: $d")
  println(s"Data per PE: $k")
  println(s"Number of Hadamard Blocks: $h")
  println(s"PEs per Hadamard: $b")


  def main(args: Array[String]): Unit = {
    println(s"Generating verilog for one Fastfood PE")
    chiselMain(Array("--backend", "v", "--targetDir", "verilog"), 
                () => Module( new DummyPE(0, bitWidth, fracWidth,
                n, p, d, false ) ) )
  }

}

object dummyArrayVerilog{

  val bitWidth = 18
  val fracWidth = 9
  
  val sigma = 11.47
  val n = 512
  val p = 128
  val d = 256

  val k = n/p
  val h = n/d 
  val b = p/h // 2

  println(s"Dictionary Size: $n")
  println(s"Number of PEs: $p")
  println(s"Number of Features: $d")
  println(s"Data per PE: $k")
  println(s"Number of Hadamard Blocks: $h")
  println(s"PEs per Hadamard: $b")


  def main(args: Array[String]): Unit = {
    println(s"Generating verilog for one Fastfood PE")

    val num = args(0).toInt // number of Dummy PEs
    val kap = args(1).toInt //length of priority link

    chiselMain(Array("--backend", "v", "--targetDir", "verilog"), 
                () => Module( new DummyArray(bitWidth, fracWidth, num, kap,
                n, p, d, false ) ) )
  }

}
