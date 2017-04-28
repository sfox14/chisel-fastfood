package pe

import Chisel._
import com.github.tototoshi.csv._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.util.Random

import fpgatidbits.ocm._

import utils._

import math._

/*
Multiple Function, Multiple Data, Merged Results
*/

class ARRAY( val bitWidth : Int, val fracWidth : Int, 
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


  val array = (0 until p).reverse.map( x => Module( new PE(x, bitWidth, 
                                              fracWidth, weights(x), n, p, d ) ) )

  val fsm = Module( new FSM( bitWidth, fracWidth, n, p, d ) )

  val fifoDepth = d*10
  val inFifo = Module( new Queue( UInt( width=bitWidth ), fifoDepth ) )
  inFifo.io.enq.bits := io.xin
  inFifo.io.enq.valid := Bool(true)
  inFifo.io.deq.ready := ( ( inFifo.io.count >= UInt( p, width=log2Up(p)+1 ) ) && fsm.io.rdy )

  // FSM inputs
  fsm.io.xin := inFifo.io.deq.bits

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

  // control signals
  for( ix <- 0 until p){
    array( ix ).io.ctrl <> fsm.io.ctrl
  }

  io.yout := array(0).io.hout + array(1).io.hout


}


class arraySim( c: ARRAY ) extends Tester( c ){

  val n = c.n
  val d = c.d
  val p = c.p

  var k = 0

  val rng = new Random( 33 )

  for( ix <- 0 until d*2 ){
    poke( c.io.xin, BigInt( rng.nextInt( 10 ) ) )
    step(1)
  }
  step(250)

}


object arrayTester{

  val bitWidth = 18
  val fracWidth = 10

  val n = 8//16
  val p = 8//2
  val d = 8

  /*
  val ram = List( List(1,0,1,0),
                  List(1,1,1,1), //) //,
                  List(0,0,1,0),
                  List(1,1,1,1),
                  List(1,0,1,0),
                  List(1,1,1,1), //) //,
                  List(0,0,1,0),
                  List(1,0,1,1)
                   )
  */
  ///*
  val ram = List( List(1,0,1,0,1,0,1,0),
                  List(1,1,1,1,1,1,1,1), //) //,
                  List(0,0,1,0,0,0,1,0),
                  List(1,1,1,1,1,1,1,1),
                  List(1,0,1,0,1,0,1,0),
                  List(1,1,1,1,1,1,1,1), //) //,
                  List(0,0,1,0,0,0,1,0),
                  List(1,0,1,1,1,0,1,1)
                   )
  //*/
  /*
  val ram = List( List(1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0),
                  List(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1), //) //,
                  List(0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0),
                  List(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
                  List(1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0),
                  List(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1), //) //,
                  List(0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0),
                  List(1,0,1,1,1,0,1,1,1,0,1,1,1,0,1,1)
                   )
  */



  def main(args: Array[String]): Unit = {
    println("Testing the FSM for controlling the array")
    
    chiselMainTest(Array("--genHarness", "--test", "--backend", "c", //"--wio", 
      "--compile", "--targetDir", ".emulator", "--vcd"), // .emulator is a hidden directory
      () => Module(new ARRAY( bitWidth, fracWidth, n, p, d, ram ) ) ) {
        f => new arraySim( f )
      }
  }
}