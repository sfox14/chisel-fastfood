package fix

import Chisel._
import com.github.tototoshi.csv._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.util.Random

import pe._

import utils._

import math._

/*
Multiple Function, Multiple Data, Merged Results
*/
// 1101110010011011
// 

class CtrlIO extends Bundle{

  val prst = Bool(OUTPUT)
  val pload = Bool(OUTPUT)
  //val pinc = Bool(OUTPUT)
  val padd = Bool(OUTPUT)
  val dload = Bool(OUTPUT)
  val dadd = Bool(OUTPUT)
  val func = UInt(OUTPUT, 4)

}

class LFSR( val init : Int ) extends Module{
  val io = new Bundle{
    val en = Bool(INPUT)
    val rst = Bool(INPUT)
    val out = Bool(OUTPUT)
  }
  val res = RegInit( UInt(init, 16) )
  val nxt_res = Cat( res(0)^res(2)^res(3)^res(5), res(15,1) )
  when( io.en ){
    res := nxt_res
  }

  when( io.rst ){
    res := UInt( init, 16 )
  }

  // 1-bit output
  io.out := res(0)

}

class ARRAYfix( val bitWidth : Int, val fracWidth : Int, 
    val n : Int, val p : Int, val d : Int, g : Seq[Seq[BigInt]], 
    s : Seq[Seq[BigInt]], alpha : Seq[Seq[BigInt]], val aStages : Int,
    cosFlag : Boolean = false, forSim : Boolean = true ) extends Module {
  
  val io = new Bundle{
    val inData = Decoupled( Vec.fill(2){ Fixed(INPUT, bitWidth, fracWidth) } ).flip
    val outData = Valid( Fixed(OUTPUT, bitWidth, fracWidth) )
  }

  Predef.assert( n>p, "Error: Only working for N>P")
  Predef.assert( p>1, "Error: P must be greater than 1")
  Predef.assert( (d & (d - 1)) == 0, "Error: X dimensionality is NOT a power of 2")
  Predef.assert( p<=d, "Error: Number of PE's should not be greater than dimensionality")
  Predef.assert( (p & (p-1)) == 0, "Error: P is NOT a power of 2" )
  Predef.assert( (n & (n-1)) == 0, "Error: N is NOT a power of 2" ) // required for this implemtation
  Predef.assert( n/p <= d, "Error: A PE can NOT compute partial WHT" )
  Predef.assert( (g.length == s.length) & (g.length== alpha.length), "Error: Parameter lengths are NOT equal")
  Predef.assert( g.length == p, "Error: Inconsistent parameter dimensions")
  val j = (d/p) // number of loads required for one x-vector
  Predef.assert( (j & (j - 1)) == 0, "Error: the number of X loads must be a power of 2")


  val array = (0 until p).reverse.map( i => Module( new PEfix(i, bitWidth, fracWidth, 
                                                    n, p, d, g(i), s(i), alpha(i), 
                                                    aStages, cosFlag, forSim ) ) )

  val fsm = Module( new FSMfix( bitWidth, fracWidth, n, p, d, aStages, cosFlag ) )

  val fifoDepth = d*10
  val readyToStart = Bool()
  val inFifo = Module( new Queue( Vec.fill(2){ Fixed( width=bitWidth, fracWidth=fracWidth ) }, fifoDepth ) ) 

  readyToStart := ( inFifo.io.count >= UInt( p, width=log2Up(p)+1 ) )

  inFifo.io.enq.bits := io.inData.bits
  inFifo.io.enq.valid := io.inData.valid
  inFifo.io.deq.ready := ( readyToStart && fsm.io.xrdy )
  
  // FSM inputs
  fsm.io.vld := readyToStart

  // connect array
  array(0).io.pin := array(p-1).io.pout 
  array(0).io.sin := array(p-1).io.sout   
  array(0).io.xin := RegNext( RegNext( inFifo.io.deq.bits(0) ) ) //fsm.io.xout
  array(0).io.yin := RegNext( RegNext( inFifo.io.deq.bits(1) ) ) //fsm.io.yout //Fixed(toFixed(-4.56, fracWidth), bitWidth, fracWidth ) ) ) //
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
    array( ix ).io.ctrl := ctrlFanOut_1 // fsm.io.ctrl //
  }

  io.inData.ready := inFifo.io.enq.ready
  io.outData.bits := array(0).io.sout //array(0).io.hout + array(1).io.hout
  io.outData.valid := fsm.io.yrdy

}

object arrayFixVerilog{

  val bitWidth = 32 //24
  val fracWidth = 24 //16
  val n = 128 //16
  val p = 32 //4
  val d = 64 //8
  val sigma = 11.47
  val aStages = 6

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
    println(s"Generating verilog for the $n/$d/$p array of PEs")
    chiselMain(Array("--backend", "v", "--targetDir", "verilog"), 
                () => Module( new ARRAYfix( bitWidth, fracWidth, n, p, d, g, s, alpha, 
                  aStages, true, false ) ) )
  }


}