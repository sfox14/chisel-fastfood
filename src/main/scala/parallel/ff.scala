package parallel

import Chisel._
import com.github.tototoshi.csv._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.util.Random

import utils._

import math._
/*
Fastfood Implementation
-----------------------
  - Forward path only
  - Computes the inference/prediction step
  - simple and fully parallel architecture
*/
class FF( val n_dicts : Int, val n_features : Int, val n_paths : Int, 
  val bitWidth : Int, val fracWidth : Int, val ram : List[List[Int]],
  val g : List[BigInt], val s : List[BigInt], val u : List[Double],
  val alpha : List[Double], forSim : Boolean = true, clk : Option[Clock] = None, 
    rst: Option[Bool] = None) extends Module( _clock=clk, _reset=rst ) {


  val io = new Bundle{

    val dIn = Decoupled( Vec.fill( n_paths ){ Fixed( INPUT, bitWidth, fracWidth ) } ).flip
    val dOut = Decoupled( Fixed( OUTPUT, bitWidth, fracWidth ) )
    //val dOut = Decoupled( Vec.fill( n_dicts){ Fixed( OUTPUT, bitWidth, fracWidth ) } )

  }

  Predef.assert( (n_dicts%n_features == 0), "Error: n_dicts is not a multiple of n_features")
  Predef.assert( (n_features & (n_features - 1)) == 0, "Error: n_features is not a power of 2")

  val n_stacks = ( n_dicts/n_features )


  // layer 1
  val layer1 = (0 until n_dicts).map( x => Module( new GPHBx(n_features, n_paths, bitWidth, fracWidth, 
                                                              ram( x ), g( x ), adderType.binAdd2, 
                                                              count.log2Up, out.direct, forSim ) ) ).toVector

  // hadamard transform
  val layer2 = ( 0 until n_stacks ).map( x => Module( new HAD( n_features, bitWidth, fracWidth ) ) )


  // scaling coefficients and cosine LUT
  val amp = sqrt(2.0/n_dicts)
  val layer3 = ( 0 until n_dicts ).map( x => Module( new LUT( bitWidth, fracWidth, s(x),
                                                              512, amp, u(x) ) ) )

  // readout dot product
  val layer4 = Module( new OUT( n_dicts, bitWidth, fracWidth ) )

  // should create some sort of ROM
  val weights = alpha.map( x => RegInit( Fixed( toFixed( x, fracWidth ), bitWidth, fracWidth ) ) )


  //connect input to layer 1 - pretty high fanout
  for( ix <- 0 until n_dicts ){
    layer1( ix ).io.dIn.bits := RegNext( RegNext( io.dIn.bits ) )
    layer1( ix ).io.dIn.valid := RegNext( RegNext( io.dIn.valid ) )

  }
  
  //connect output of layer1 to layer2 input
  val layer1_to_2 = layer1.grouped( n_features ).toVector
  var iz = 0
  for( ix <- 0 until n_stacks ){
    for( iy <- 0 until n_features ){
      layer2( ix ).io.dIn( iy ) := layer1_to_2( ix )( iy ).io.dOut.bits

      layer3( iz ).io.dIn := RegNext( RegNext( RegNext( RegNext( layer2( ix ).io.dOut( iy ) ) ) * 
                                      RegNext( RegInit( Fixed( s(iz), bitWidth, fracWidth ) ) ) ) )
      
      // hook up the last layer dot product
      layer4.io.dIn( iz ) := layer3( iz ).io.dOut
      layer4.io.alpha( iz ) := weights( iz )
      io.dOut.bits := layer4.io.dOut 
      iz = iz + 1
    } 
  }

  io.dOut.valid := ShiftRegister( layer1(0).io.dOut.valid, log2Up( n_features ) + 2 + 4 +
                                    layer4.n_cycles )

}


