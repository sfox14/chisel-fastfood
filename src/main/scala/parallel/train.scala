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
  - includes training
  - weights are updated incrementally for each input
  - the weights must be updated before the next input arrives at layer4 (readout)
  - we can stall the pipeline by slowing down our IO. eg. if layer4 takes 10 cycles and 
    the update step takes 5 cycles, we should stream one input every 15 cycles at most.
  - tested on 64 features and 128 n_dicts
*/

class TRAIN( val n_dicts : Int, val n_features : Int, val n_paths : Int, 
  val bitWidth : Int, val fracWidth : Int, val ram : List[List[Int]],
  val g : List[BigInt], val s : List[BigInt], val u : List[Double],
  val alpha : List[Double], forSim : Boolean = true ) extends Module {


  val io = new Bundle{

    val dIn = Decoupled( Vec.fill( n_paths ){ Fixed( INPUT, bitWidth, fracWidth ) } ).flip
    val dOut = Decoupled( Fixed( OUTPUT, bitWidth, fracWidth ) )
    val yIn = Valid( Fixed( INPUT, bitWidth, fracWidth ) ).flip

  }

  Predef.assert( (n_dicts%n_features == 0), "Error: n_dicts is not a multiple of n_features")
  Predef.assert( (n_features & (n_features - 1)) == 0, "Error: n_features is not a power of 2")

  val n_stacks = ( n_dicts/n_features )


  // layer 1
  val layer1 = (0 until n_dicts).map( x => Module( new GPHBx(n_features, n_paths, bitWidth, fracWidth, 
                                                              ram( x ), g( x ), adderType.binAdd3, 
                                                              count.log2Up, out.direct, forSim ) ) ).toVector

  // hadamard transform
  val layer2 = ( 0 until n_stacks ).map( x => Module( new HAD( n_features, bitWidth, fracWidth ) ) )


  // scaling coefficients and cosine LUT
  val amp = sqrt(2.0/n_dicts)
  val layer3 = ( 0 until n_dicts ).map( x => Module( new LUT( bitWidth, fracWidth, s(x),
                                                              512, amp, u(x) ) ) )

  // readout dot product
  val layer4 = Module( new OUT( n_dicts, bitWidth, fracWidth ) )

  val weights = alpha.map( x => RegInit( Fixed( toFixed( x, fracWidth ), bitWidth, fracWidth ) ) )
  val kernel = (0 until n_dicts).map( x => RegInit( Fixed(0, width=bitWidth, fracWidth=fracWidth ) ) )

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
      //io.dOut.bits( iz ) := layer2( ix ).io.dOut( iy )
      layer3( iz ).io.dIn := RegNext( RegNext( layer2( ix ).io.dOut( iy ) ) * 
                                      RegInit( Fixed( s(iz), bitWidth, fracWidth ) ) )
      
      kernel( iz ) := layer3( iz ).io.dOut
      // hook up the last layer dot product
      layer4.io.dIn( iz ) := kernel( iz )
      layer4.io.alpha( iz ) := weights( iz )
      io.dOut.bits := layer4.io.dOut
      iz = iz + 1
    } 
  }

  io.dOut.valid := ShiftRegister( layer1(0).io.dOut.valid, log2Up( n_features ) + 2 + 2 +
                                    layer4.n_cycles + 1 ) //+1 for queue

  val yFifo = Module( new Queue( Fixed( width=bitWidth, fracWidth=fracWidth), 32 ) )
  yFifo.io.enq.valid := io.yIn.valid
  yFifo.io.enq.bits := io.yIn.bits
  
  yFifo.io.deq.ready := io.dOut.valid
  val y = yFifo.io.deq.bits

  val err = Reg( init = Fixed(0, bitWidth, fracWidth), next = ( y-io.dOut.bits ) )
  
  val eta = RegInit( Fixed( toFixed( 1.0, fracWidth), bitWidth, fracWidth ) )
  val etaError = RegNext( eta *% RegNext( err )  )

  val update = ( 0 until n_dicts ).map( x => RegNext( RegNext( etaError ) *% RegNext( kernel( x ) ) ) )
 
  val vldUpdate = ShiftRegister( io.dOut.valid, 1 + 2 + 2 ) //err + etaError + update stages


  when( vldUpdate ){
    for( ix <- 0 until n_dicts ){ weights( ix ) := weights( ix ) + update( ix ) }    
  }.otherwise{
    for( ix <- 0 until n_dicts ){ weights( ix ) := weights( ix ) }
  }



}


