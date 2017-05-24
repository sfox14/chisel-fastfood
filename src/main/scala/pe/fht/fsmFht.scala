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
  val eStages = 3 // extra pipeline stages
  val mStages = 4 // read/write ram latency
  val k = n/p   // Dictionaries per PE
  val h = (n/d) // number of had blocks
  val b = p/h // PEs per had block
  
  // state registers
  val global = RegInit( UInt(0, width=2) )
  val state = RegInit( UInt(15, width=4) )
  val nextState = RegInit( UInt(2, width=4) )
  val loading = RegInit( Bool(false) )

  // control signal registers (9-bit)
  val ready = RegInit( Bool(false) )
  val outReady = RegInit( Bool(false) )
  val pLoad = RegInit( Bool(false) )
  val pAdd = RegInit( Bool(false) )
  val dFunc = RegInit( UInt(0, width=4) )
  val sX = RegInit( UInt(4, width=3) )

  // FSM counters
  val pcK = RegInit( UInt(0, width=log2Up( k )) )
  val pcD = RegInit( UInt(0, width=log2Up( d )) )
  val pcB = RegInit( UInt(0, width=log2Up( b )+1) )
  val pcH = RegInit( UInt(0, width=log2Up( h )+1) )

  val pcBL = RegInit( UInt(0, width=log2Up( log2Up( b ) )+1 ) )
  val pcKL = RegInit( UInt(0, width=log2Up( log2Up( k ) )+1 ) )
  
  val pcL = RegInit( UInt(0, width=18 ) ) // 2**18 maximum cycles
  val pcE = RegInit( UInt(0, width=log2Up(aStages+2)+1) ) //extra counter

  val latency_had = k*( log2Up(b) + log2Up(k) )
  val latency_extra = h + (aStages+1+1) + (aStages+1) + (log2Up(b) + 1)
  val latency = latency_had*2 + k*7 + latency_extra

  println(s"LATENCY: $latency")
  println(s"THROUGHPUT: $latency")

  
  when( global === UInt(0) ){
    // there is p data in the fifo
    when( io.vld ){
      loading := Bool(true)
    }
  }

  when( global === UInt(1) ){


    when( state === UInt(8) ){
      // multiply B using LFSR
      dFunc := UInt(8)
      pcK := pcK + UInt(1)
      // do not load more data
      ready := Bool(false)

      when( pcK === UInt(k - 1) ){
        // move to state 0, hadamard
        state := UInt(0)
        nextState := UInt(2)
        dFunc := UInt(0)
        pcK := UInt(0)
        pLoad := Bool(false)
        pAdd := Bool(false) // end 8
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
        pcBL := pcBL + UInt(1)
        when( pcBL === UInt( log2Up(b) -1 ) ){
          // move to hadamard transform, stage 2
          state := UInt(1)
          dFunc := UInt(1)
          pAdd := Bool(false) // end of dFunc=0
          pAdd := Bool(true) // start of dFunc=1
          pcBL := UInt(0)
          sX := UInt(4) // keep data local
        }
        pcK := UInt(0)
      }
    }

    when( state === UInt(1) ){
      // hadamard transform, stage 2
      dFunc := UInt(1)
      pcK := pcK + UInt(1)
      pAdd := Bool(true)
      sX := UInt(4)

      when( pcK === UInt( k-1 ) ){
        pcKL := pcKL + UInt(1)
        when( pcKL === UInt( log2Up(k)-1 ) ){

          // Finished, choose whether to go to G, or ShiftRegister
          when( nextState === UInt(2) ){
            // start gausian multiply
            state := UInt(2)
            dFunc := UInt(2)
            pAdd := Bool(true)
            // start the next hadamard switch
            sX := UInt(0)
          }
          when( nextState === UInt(3) ){
            // start, s multiply
            state := UInt(3)
            dFunc := UInt(3)
            pAdd := Bool(true)
          }
          pcKL := UInt(0)
        }
        pcK := UInt(0)
      }
    }

    when( state === UInt(2) ){
      // multiply gaussian stage
      dFunc := UInt(2)
      pcK := pcK + UInt(1)
      pAdd := Bool(true)

      when( pcK === UInt( k-1 ) ){
        // move to hadamard again 
        state := UInt(0)
        nextState := UInt(3)
        dFunc := UInt(0)
        pcK := UInt(0)

        pAdd := Bool(false) // end 2
        pAdd := Bool(true) // start 0
        sX := sX + UInt(1)
      }
    }

    when( state === UInt(3) ){
      // multiply s
      dFunc := UInt(3)
      pcK := pcK + UInt(1)
      pAdd := Bool(true)

      when( pcK === UInt( k-1 ) ){
        // start, cosine function
        state := UInt(5)
        dFunc := UInt(5)
        pAdd := Bool(true)
      }
    }

    when( state === UInt(5) ){
      // cosine function
      dFunc := UInt(5)
      pcK := pcK + UInt(1)
      pAdd := Bool(true)

      when( pcK === UInt( k-1 ) ){
        // start, multiply alpha
        state := UInt(4)
        dFunc := UInt(4)
        pAdd := Bool(true)
      }
    }

    when( state === UInt(4) ){
      // multiply alpha
      dFunc := UInt(4)
      pcK := pcK + UInt(1)
      pAdd := Bool(true)

      when( pcK === UInt( k-1 ) ){
        // bail
        state := UInt(6)
        dFunc := UInt(6)
        pAdd := Bool(false)
      }
    }


    when( state === UInt(6) ){
      // pe is idle while we compute the sum
      dFunc := UInt(6)
      pcBL := pcBL + UInt(1)
      // wait log2Up( b ) cycles for adder tree and 1 cycle for sum_local
      when( pcBL === UInt( log2Up( b ) - 1 + 1 ) ){
        // move to global sum stage
        dFunc := UInt(7)
        state := UInt(7)
        pcBL := UInt(0)
      }
    }

    when( state === UInt(7) ){
      // pe is idle while we compute global sum
      dFunc := UInt(7)
      pcH := pcH + UInt(1)
      when( pcH === UInt( h ) ){
        // move to computing the error
        dFunc := UInt(14)
        state := UInt(14)
        outReady := Bool(true)
        pcH := UInt(0)
      }
    }

    when( state === UInt(14) ){
      // compute error, and hold for aluCode - opCode, aStages + 1
      dFunc := UInt(14)
      outReady := Bool(false)
      pcE := pcE + UInt(1)
      when( pcE === UInt( aStages+1 ) ){
        pcE := UInt(0)
        dFunc := UInt(15)
        state := UInt(15) 
      }
    }

    when( state === UInt(15) ){
      // compute delta, err*eta
      dFunc := UInt(15)
      pcE := pcE + UInt(1)
      when( pcE === UInt( aStages+1-1 ) ){
        pcE := UInt(0)
        dFunc := UInt(13)
        state := UInt(13)
        pAdd := Bool(true)
      }
    }

    when( state === UInt(13) ){
      // kernel * delta
      dFunc := UInt(13)
      pcK := pcK + UInt(1)
      pAdd := Bool(true)
      when( pcK === UInt( k-1 ) ){
        pcK := UInt(0)
        dFunc := UInt(12)
        state := UInt(12)
        pAdd := Bool(true)
      }
    }

    when( state === UInt(12) ){
      // update alpha
      dFunc := UInt(12)
      pcK := pcK + UInt(1)
      when( pcK === UInt( k-1 ) ){
        pcK := UInt(0)
        dFunc := UInt(9)
        state := UInt(9)
        pAdd := Bool(false)
      }
    }

    when( state === UInt(9) ){
      dFunc := UInt(9)
    }




    // wait for time to load
    pcL := pcL + UInt(1)
    when( pcL === UInt( latency - d ) ){
      loading := Bool(true)
    }
  }

  // load data from inFifo
  when( loading ){

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

      pcD := UInt(0)
      // start LFSR, and hin write-back via switch
      pAdd := Bool(true)
      sX := UInt(0)
      
      loading := Bool(false)

    }

  }

  io.xrdy := ShiftRegister( ready, mStages - 1 )
  io.yrdy := ShiftRegister( outReady, 1 + mStages + eStages + aStages + 1 ) //fsm to aluCode

  //io.ctrl.pe.prst := RegNext(pRst)
  io.ctrl.pe.pload := ShiftRegister( pLoad, mStages - 1 )
  io.ctrl.pe.padd := pAdd
  io.ctrl.pe.func := dFunc
  io.ctrl.pe.sx := ShiftRegister( sX, mStages + eStages + aStages ) //delay through pipeline, mStages + 1 + aStages + 1

}