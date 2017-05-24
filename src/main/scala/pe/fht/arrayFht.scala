package fht

import Chisel._
import com.github.tototoshi.csv._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.util.Random

//import pe._

import utils._

import math._

/*
Multiple Function, Multiple Data, Merged Results
*/

class CtrlPE extends Bundle{
  // PE control signals
  val pload = Bool(OUTPUT)
  val padd = Bool(OUTPUT)
  val func = UInt(OUTPUT, 4)
  val sx = UInt(OUTPUT, 3)

}

class CtrlIO extends Bundle{

  val pe = new CtrlPE

  override def clone = { new CtrlIO().asInstanceOf[this.type] }

}

class LFSR( val init : Int ) extends Module{
  val io = new Bundle{
    val en = Bool(INPUT)
    val out = Bool(OUTPUT)
  }
  val res = RegInit( UInt(init, 16) )
  val nxt_res = Cat( res(0)^res(2)^res(3)^res(5), res(15,1) )
  when( io.en ){
    res := nxt_res
  }
  // reset
  when( !io.en ){
    res := UInt( init, 16 )
  }

  // 1-bit output
  io.out := res(0)

}

class ARRAYfht( val bitWidth : Int, val fracWidth : Int, 
    val n : Int, val p : Int, val d : Int, g : Seq[Seq[Seq[BigInt]]], 
    s : Seq[Seq[Seq[BigInt]]], alpha : Seq[Seq[Seq[BigInt]]], val aStages : Int,
      forSim : Boolean = true ) extends Module {
  
  val io = new Bundle{
    val inData = Decoupled( Vec.fill(2){ Fixed(INPUT, bitWidth, fracWidth) } ).flip
    val outData = Valid( Fixed(OUTPUT, bitWidth, fracWidth) )
  }

  Predef.assert( n>p, "Error: Only working for N>P")
  Predef.assert( p>1, "Error: P must be greater than 1")
  Predef.assert( (d & (d - 1)) == 0, "Error: X dimensionality is NOT a power of 2")
  Predef.assert( p<=d, "Error: Number of PE's should not be greater than dimensionality")
  //Predef.assert( (p & (p-1)) == 0, "Error: P is NOT a power of 2" )
  //Predef.assert( (n & (n-1)) == 0, "Error: N is NOT a power of 2" ) // required for this implemtation
  Predef.assert( n/p <= d, "Error: A PE can NOT compute partial WHT" )

  // Dictionaries per PE, Hadamard Blocks and PEs per Hadamard
  val k = n/p
  val h = (n/d) 
  val b = p/h

  Predef.assert( g.length == h, "Error: Parameter vectors have wrong shape" )
  Predef.assert( g(0).length == b, "Error: Parameter vectors have wrong shape " )
  Predef.assert( b == 4 | b == 8 | b == 16, "Error: Block of PEs not supported" )


  val array = (0 until h).map( i => Module( new HadBlock( bitWidth, fracWidth, n, p, d, g(i),
                                                s(i), alpha(i), aStages, forSim ) ))

  val fsm = Module( new FSMfht( bitWidth, fracWidth, n, p, d, aStages ) )

  val fifoDepth = d*10
  val readyToStart = Bool()
  val inFifo = Module( new Queue( Vec.fill(2){ Fixed( width=bitWidth, fracWidth=fracWidth ) }, fifoDepth ) ) 

  readyToStart := ( inFifo.io.count >= UInt( d-1, width=log2Up(d)+1 ) )

  inFifo.io.enq.bits := io.inData.bits
  inFifo.io.enq.valid := io.inData.valid
  inFifo.io.deq.ready := ( readyToStart && fsm.io.xrdy )
  
  // FSM inputs
  fsm.io.vld := readyToStart
  
  //fsm.io.vld := Bool(true)
  //array(0).io.xin := RegNext( RegNext( io.inData.bits(0) ) )
  //array(0).io.yin := RegNext( RegNext( io.inData.bits(1) ) )

    // connect array   
  array(0).io.xin := RegNext( RegNext( inFifo.io.deq.bits(0) ) ) //fsm.io.xout
  array(0).io.yin := RegNext( RegNext( inFifo.io.deq.bits(1) ) ) //fsm.io.yout
  for (ix <- 1 until h){
    array(ix).io.xin := array(ix-1).io.xout
    array(ix).io.yin := array(ix-1).io.yout
  }

  // connect sum
  array(0).io.sin := array(h-1).io.sout
  for( ix <- 1 until h ){
    array(ix).io.sin := array(ix-1).io.sout
  }

  // control signals
  array(0).io.ctrl := RegNext( RegNext( fsm.io.ctrl ) )
  for( ix <- 1 until h ){
    array( ix ).io.ctrl := array( ix-1 ).io.ctrlOut 
  }

  io.inData.ready := inFifo.io.enq.ready
  io.outData.bits := array(0).io.sout //array(0).io.hout + array(1).io.hout
  io.outData.valid := fsm.io.yrdy



}