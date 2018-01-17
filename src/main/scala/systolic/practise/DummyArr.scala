package practise

import Chisel._
import com.github.tototoshi.csv._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.util.Random

import utils._
import xilinx._

import math._



// Example 1 in paper

class DummyArray( val bitWidth : Int, val fracWidth : Int, val num : Int,
        val kap : Int, val n : Int, val p : Int, val d : Int, forSim : Boolean = true, 
        seed : Int = 45687 ) extends Module{
  
  val io = new Bundle{
    val xin = Fixed(INPUT, bitWidth, fracWidth)
    val xout = Fixed(OUTPUT, bitWidth, fracWidth)
    
    val hin = Fixed(INPUT, bitWidth, fracWidth)
    val hout = Fixed(OUTPUT, bitWidth, fracWidth)
    
    val ctrl = new CtrlPE().flip()
    val ctrlOut = new CtrlPE()

  }

  val array = (0 until num).map(i => Module( new DummyPE(i, bitWidth, fracWidth, n, p, 
                                              d, forSim)))

  val kd = kap-1

  array(0).io.xin := io.xin
  array(0).io.hin := io.hin
  array(0).io.ctrl := io.ctrl
  for( ix <- 1 until num ){
    array(ix).io.xin := array(ix-1).io.xout
    array(ix).io.hin := array(ix-1).io.hout
    array(ix).io.ctrl := array(ix-1).io.ctrlOut
  }
  // priority link
  array( kd ).io.kin := array(0).io.hout

  // connect output
  io.hout := array( num-1 ).io.hout
  
}



