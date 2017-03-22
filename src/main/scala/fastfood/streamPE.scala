package fastfood

import Chisel._
import com.github.tototoshi.csv._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.util.Random

import utils._

import math._


class StreamPE( val n_features : Int, val bitWidth : Int, val fracWidth : Int, 
  val weights : List[Int], val col : Int, val s : Int ) extends Module {


  val io = new Bundle{

    val dIn = Decoupled( Fixed( INPUT, bitWidth, fracWidth ) ).flip
    val outValid = Bool( INPUT )
    val ringValid = Bool( INPUT )

    val dPass = Decoupled( Fixed( OUTPUT, bitWidth, fracWidth ) )
    val nextOutValid = Bool( OUTPUT )
    val ringOutValid = Bool( OUTPUT )

    val dOut = Valid( Fixed( OUTPUT, bitWidth, fracWidth ) )

  }

  // pass to next PE on next cycle
  io.dPass <> RegNext( io.dIn )
  io.nextOutValid := RegNext( io.outValid )
  io.ringOutValid := RegNext( io.ringValid )

  val accumulator = RegInit( Fixed(0, bitWidth, fracWidth) )
  //val outBuff = RegInit( Fixed(0, bitWidth, fracWidth) )

  //val counter = RegInit( UInt(0, width = log2Up( n_features + 1 )) )
  when( io.dIn.valid ){
    //counter := counter + UInt(1,1)
    accumulator := accumulator + io.dIn.bits
  }

  //val outValid = ( counter === UInt( n_features ) )
  when( io.outValid ){
    //counter := UInt(0)
    when( io.dIn.valid ){
        accumulator := io.dIn.bits
      }.otherwise{
        accumulator := Fixed(0, bitWidth, fracWidth)
      }
    
    //outBuff := accumulator
  }

  io.dOut.bits := accumulator
  io.dOut.valid := io.outValid
  

}


