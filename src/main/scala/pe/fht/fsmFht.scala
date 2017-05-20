package fht

import Chisel._
import com.github.tototoshi.csv._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.util.Random

import utils._

import math._

/*
Finite State Machine:
  - fixed point
  - does not use ctrl.io.pinc
  - Fast Hadamard Transform (FHT)
*/


class FSMfht( val bitWidth : Int, val fracWidth : Int, 
    val n : Int, val p : Int, val d : Int, 
    val aStages : Int ) extends Module{

  val io = new Bundle{

    val vld = Bool(INPUT)
    val xrdy = Bool(OUTPUT)
    val yrdy = Bool(OUTPUT)

    val ctrl = new CtrlIO()
  }


  val rng = new Random( 21 )

  // architecture parameters
  val mStages = 1 // read/write ram latency
  val k = n/p   // Dictionaries per PE
  val h = (n/d) // number of had blocks
  val b = p/h // PEs per had block
  
  // state registers
  val global = RegInit( UInt(0, width=2) )
  val initial = RegInit( UInt(0, width=2) )
  val state = RegInit( UInt(0, width=4) )
  val stall = RegInit( Bool(false) )
  val waiting = RegInit( Bool(false) )

  // control signal registers (9-bit)
  val ready = RegInit( Bool(false) )
  val outReady = RegInit( Bool(false) )
  val pLoad = RegInit( Bool(false) )
  val pAdd = RegInit( Bool(false) )
  val dFunc = RegInit( UInt(0, width=4) )
  val sX = RegInit( UInt(0, width=3) )


  // FSM counters
  val pcN = RegInit( UInt(0, width=log2Up( n )) )
  val pcP = RegInit( UInt(0, width=log2Up( p )) )
  val pcK = RegInit( UInt(0, width=log2Up( k )) )
  val pcDP = RegInit( UInt(0, width=log2Up( d/p )) )
  val pcD = RegInit( UInt(0, width=log2Up( d )) )
  val pcB = RegInit( UInt(0, width=log2Up( b )) )

  
  when( global === UInt(0) ){
    // there is p data in the fifo
    when( io.vld ){

      ready := Bool(true)
      pLoad := Bool(false)

      pcB := pcB + UInt(1)
      pcD := pcD + UInt(1)
      

      when( pcB === UInt( b -1 ) ){
        pLoad := Bool(true)
        pcB := UInt(0)
      }

      // the first had block is full
      when( pcD === UInt( d -1 ) ){
        /* 
        - move to compute stage, global=1
        - multiply B, dFunc=2
        - keep pLoad high for k cycles
        */
        global := UInt(1)
        state := UInt(8)
        dFunc := UInt(8)
        // do not load more data
        ready := Bool(false)
        pcD := UInt(0)
        // start LFSR, and hin write-back via switch
        pAdd := Bool(true)
        sX := UInt(1)
      
      }

    }
  }

  when( global === UInt(1) ){

    when( state === UInt(8) ){
      // multiply B using LFSR
      dFunc := UInt(8)
      pcK := pcK + UInt(1)

      when( pcK === UInt(k - 1) ){
        // move to state 0, hadamard
        state := UInt(0)
        dFunc := UInt(0)
        pcK := UInt(0)
        pLoad := Bool(false)
        pAdd := Bool(false) // end 2
        pAdd := Bool(true) // start 0
        sX := sX + UInt(1)
      }
    }


    when( state === UInt(0) ){
      // hadamard transform, stage 1: switching network
      dFunc := UInt(0)
      pcK := pcK + UInt(1)

      when( pcK === UInt( k -1 ) ){
        // change ctrl signals
        sX := sX + UInt(1)
        pcB := pcB + UInt(1)
        when( pcB === UInt( log2Up(b) -1 ) ){
          // move to hadamard transform, stage 2
          state := UInt(1)
          dFunc := UInt(1)
          pAdd := Bool(false) // end of dFunc=0
          pAdd := Bool(true) // start of dFunc=1
          pcB := UInt(0)
          sX := UInt(0) // keep data local
        }
        pcK := UInt(0)
      }
    }

    when( state === UInt(1) ){
      // hadamard transform, stage 2
      dFunc := UInt(1)
      pcK := pcK + UInt(1)
      pAdd := Bool(true)
      sX := UInt(0)

      when( pcK === UInt( k-1 ) ){
        pcP := pcP + UInt(1)
        when( pcP === UInt( log2Up(k)-1 ) ){
          // finished
          state := UInt(7)
          dFunc := UInt(7)
          pAdd := Bool(false)
          pcP := UInt(0)
        }
        pcK := UInt(0)
      }
    }

    when( state === UInt(7) ){
      dFunc := UInt(7)
    }

  }



  io.xrdy := ShiftRegister( ready, mStages - 1 ) //mStages - 1 )
  io.yrdy := outReady //dFunc to aluCode

  //io.ctrl.pe.prst := RegNext(pRst)
  io.ctrl.pe.pload := ShiftRegister( pLoad, mStages - 1 ) //mStages - 1 )
  io.ctrl.pe.padd := ShiftRegister( pAdd, 0 ) //mStages )

  io.ctrl.pe.func := dFunc

  io.ctrl.pe.sx := ShiftRegister( sX, mStages + 2 ) //2 = delay through pipeline, mStages + 1 + aStages + 1

}