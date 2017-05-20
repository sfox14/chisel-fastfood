package pe

import Chisel._
import com.github.tototoshi.csv._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.util.Random

import utils._

import math._

/*
Multiple Function, Multiple Data, Merged Results
*/

class ARRAYpr( val bitWidth : Int, val fracWidth : Int, 
    val n : Int, val p : Int, val d : Int, val ram : List[List[Int]] ) extends Module {
  val io = new Bundle{
    val xin = UInt(INPUT, bitWidth) //Fixed(INPUT, bitWidth, fracWidth)
    val yin = UInt(INPUT, bitWidth)

    val yout = UInt(OUTPUT, bitWidth)
  }

  Predef.assert( p>1, "Error: P must be greater than 1")
  Predef.assert( (d & (d - 1)) == 0, "Error: X dimensionality is NOT a power of 2")
  Predef.assert( p<=d, "Error: Number of PE's should not be greater than dimensionality")
  Predef.assert( (p & (p-1)) == 0, "Error: P is NOT a power of 2" )
  Predef.assert( (n & (n-1)) == 0, "Error: N is NOT a power of 2" ) // required for this implemtation
  Predef.assert( n/p <= d, "Error: A PE can NOT compute partial WHT" )
  Predef.assert( ram.length == d, "Error: D and RAM length does NOT match")
  Predef.assert( ram(0).length == n, "Error: N and RAM(0) length does NOT match")
  val j = (d/p) // number of loads required for one x-vector
  Predef.assert( (j & (j - 1)) == 0, "Error: the number of X loads must be a power of 2")



  val weights = ram.grouped( p ).toList
                  .reduce(_.zip(_).map(x => List(x._1, x._2).flatten ))
                  .reduce( _++_ ).grouped( j*n ).toList // groups j*n neurons per node


  val array = (0 until p).reverse.map( i => Module( new PEr1(i, bitWidth, 
                                              fracWidth, weights(i), n, p, d ) ) )

  val fsm = Module( new FSMr1( bitWidth, fracWidth, 1024, 256, 512 ) )

  /*
  val fifoDepth = d*10
  val inFifo = Module( new Queue( UInt( width=bitWidth ), fifoDepth ) )
  inFifo.io.enq.bits := io.xin
  inFifo.io.enq.valid := Bool(true)
  inFifo.io.deq.ready := ( ( inFifo.io.count >= UInt( p, width=log2Up(p)+1 ) ) && fsm.io.rdy )

  // FSM inputs
  fsm.io.xin := inFifo.io.deq.bits //RegNext( RegNext( inFifo.io.deq.bits ) )
  fsm.io.yin := UInt(12686, bitWidth) //RegNext( RegNext( UInt(12686, bitWidth) ) ) // UInt(22290, bitWidth) // 249458
  fsm.io.vld := inFifo.io.deq.valid //RegNext( RegNext( inFifo.io.deq.valid ) )
  */
  fsm.io.xin := io.xin
  fsm.io.yin := UInt(12686, bitWidth)
  fsm.io.vld := Bool(true)

  // connect array
  array(0).io.pin := array(p-1).io.pout 
  array(0).io.sin := array(p-1).io.sout   
  array(0).io.xin := fsm.io.xout
  array(0).io.yin := fsm.io.yout
  for (ix <- 1 until p){
    array(ix).io.pin := array(ix-1).io.pout
    array(ix).io.sin := array(ix-1).io.sout
    array(ix).io.xin := array(ix-1).io.xout
    array(ix).io.yin := array(ix-1).io.yout
  }
  // connect the hadamard loopback:
  val hadArrays = array.grouped( d*p/n ).toList
  val ld = d*p/n  // number of PEs per hadamard
  for( ix <- 0 until hadArrays.length ){
    hadArrays(ix)(0).io.hin := hadArrays(ix)(ld-1).io.hout
    for( iy <- 1 until ld){
      hadArrays(ix)(iy).io.hin := hadArrays(ix)(iy-1).io.hout
    }
  }

  val ctrlFanOut_0 = RegNext( fsm.io.ctrl )
  val ctrlFanOut_1 = RegNext( ctrlFanOut_0 )

  // control signals
  for( ix <- 0 until p){
    array( ix ).io.ctrl := ctrlFanOut_1 //ShiftRegister( fsm.io.ctrl, 4 )
  }

  io.yout := array(0).io.sout + array(23).io.sout


}




object arrayPrVerilog{

  val bitWidth = 18
  val fracWidth = 10
  val n = 1024 //128 //1024 //128 //16
  val p = 256 //32 //256 //16 //4
  val d = 512 //64 //512 //64 //8

  val rng = new Random( 33 )
  val ram = ( 0 until d ).map( x => (0 until n).map( y => rng.nextInt(2) ).toList ).toList


  def main(args: Array[String]): Unit = {
    println("Generating verilog for the 4-4-4 array of PEs")
    chiselMain(Array("--backend", "v", "--targetDir", "verilog"), 
                () => Module( new ARRAYpr( bitWidth, fracWidth, n, p, d, ram ) ) ) //PE42( 0, bitWidth, fracWidth, weights, n, p, d ) ) )
  }


}