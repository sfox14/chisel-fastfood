package pe

import Chisel._
import com.github.tototoshi.csv._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.util.Random

import fpgatidbits.ocm._

import utils._

import math._



class FSMr1( val bitWidth : Int, val fracWidth : Int, 
    val n : Int, val p : Int, val d : Int ) extends Module{

  val io = new Bundle{
    val xin = UInt(INPUT, bitWidth)
    val yin = UInt(INPUT, bitWidth)
    val vld = Bool(INPUT)
    val rdy = Bool(OUTPUT)
    val yout = UInt(OUTPUT, bitWidth)
    val xout = UInt(OUTPUT, bitWidth)

    val ctrl = new CtrlIO()
  }

  

  val rng = new Random( 21 )

  // architecture parameters
  val regIn = 1
  val regOut = 2
  val mStages = 2*regIn + regOut //mem stages 
  val oStages = 1 // operand before alu
  val aStages = 3 // alu

  val hStages =  max(-1, n/p -1 - oStages - aStages) // length of hReg
  val toStall = ( hStages == -1 ) // true or false
  val sLen = abs( n/p -1 - oStages - aStages ) //stall cycles

  val dStages = mStages + n/p -2 //cycles that dpath2 starts ahead of pLoad

  //val iCycles = max( mStages + 1, (n/p)+1 ) // stages between dFunc=0 and dFunc=3 
  val sCycles = 100 // stall dpath1, parameterise this


  // state registers
  val global = RegInit( UInt(0, width=2) )
  val initial = RegInit( UInt(0, width=2) )
  val state = RegInit( UInt(0, width=4) ) //dpath2
  val stall = RegInit( Bool(false) )
  val waiting = RegInit( Bool(false) )

  // control signal registers (9-bit)
  val ready = RegInit( Bool(false) )
  val pRst = RegInit( Bool(false) )
  val pLoad = RegInit( Bool(false) )
  val pInc = RegInit( Bool(false) )
  val pAdd = RegInit( Bool(false) )
  val dLoad = RegInit( Bool(false) )
  val dAdd  = RegInit( Bool(false) )
  val dFunc = RegInit( UInt(0, width=3) )


  // output nets and default connections
  val xout = UInt(width=bitWidth)
  xout := io.xin
  val yout = UInt(width=bitWidth)
  yout := io.yin


  // FSM counters
  val pcN = RegInit( UInt(0, width=log2Up( n )) )
  val pcP = RegInit( UInt(0, width=log2Up( p )) )
  val pcNP = RegInit( UInt(0, width=log2Up( n/p )) )
  val pcD = RegInit( UInt(0, width=log2Up( d/p ) ))
  
  val sDelay = RegInit( UInt(0, log2Up( sCycles ) ))
  
  val dcN = RegInit( UInt(0, log2Up( n ) ))
  //val dcI = RegInit( UInt(0, log2Up( iCycles ) ))
  val dcD = RegInit( UInt(0, log2Up( d ) ))
  val dcHad = RegInit( UInt(0, log2Up( d*n/p ) )) //hadamard


  // pLoad g, s, alpha from ROM
  val bram = Vec( (0 until 3*n).map( x => UInt( BigInt( rng.nextInt(5) ), width=bitWidth ) ) ) 
  val iAddr = RegInit( UInt( 0, width=3*n-1 ) )
  val gsa = ShiftRegister( bram( iAddr ), regOut)
  


  // Stage 0: Start reading from gsa BRAM regIn (i.e. 2) cycles early 

  when( global === UInt(0) ){
    // initial stage, start gsa before global == 1, master/system reset
    iAddr := iAddr + UInt(1)
    xout := gsa( iAddr )
    when( iAddr === UInt(2-1) ){
      global := UInt(1)
    }
  }

  // Stage 1: Initialiasation

  when( global === UInt(1) ){
    iAddr := iAddr + UInt(1)
    when( initial === UInt(0) ){
      // pLoad/init g
      xout := gsa
      pcN := pcN + UInt(1)
      pcP := pcP + UInt(1)
      dLoad := Bool(false)
      dAdd := Bool(false)
      dFunc := UInt(0)
      when( pcP === UInt( p-1 ) ){
        dLoad := Bool(true)
        dAdd := Bool(true) // for n==p, addr := addr (below)
        pcNP := pcNP + UInt(1)
      }
      when( pcNP === UInt( n/p -1 ) && pcP === UInt( p-1 ) ){
        //dFunc := UInt(1)
        initial := UInt(1)
      }  
    }

    when( initial === UInt(1) ){
      // pLoad/init S
      xout := gsa
      pcN := pcN + UInt(1)
      pcP := pcP + UInt(1)
      dLoad := Bool(false)
      dAdd := Bool(false)
      dFunc := UInt(1)
      when( pcP === UInt( p-1 ) ){
        dLoad := Bool(true)
        dAdd := Bool(true) // for n==p, addr := addr (below)
        pcNP := pcNP + UInt(1)
      }
      when( pcNP === UInt( n/p -1 ) && pcP === UInt( p-1 ) ){
        //dFunc := UInt(2)
        initial := UInt(2)
      }
    }

    when( initial === UInt(2) ){
      // pLoad/init alpha
      xout := gsa
      pcN := pcN + UInt(1)
      pcP := pcP + UInt(1)
      dLoad := Bool(false)
      dAdd := Bool(false)
      dFunc := UInt(2)
      when( pcP === UInt( p-1 ) ){
        dLoad := Bool(true)
        dAdd := Bool(true) // for n==p, addr := addr (below)
        pcNP := pcNP + UInt(1)
      }
      when( pcNP === UInt( n/p -1 ) && pcP === UInt( p-1 ) ){
        //dFunc := UInt(7)
        //dAdd := Bool(false)
        initial := UInt(3)
        pcN := UInt(0) //make sure
        ready := Bool(true) //ready for xin
      }
    }

    when( initial === UInt(3) ){
      // pLoad/init xin, yin
      xout := io.xin
      yout := io.yin
      pcP := pcP + UInt(1)
      dLoad := Bool(false)
      dAdd := Bool(false)
      pLoad := Bool(false)
      dFunc := UInt(7)
      when( pcP === UInt( p-1 ) ){
        // pLoad, and start pAdd counter
        pLoad := Bool(true)
        pAdd := Bool(true)
        global := UInt(2)
        ready := Bool(false)
        if( n==p ){
          ready := Bool(true)
        }
      }
    }

  }
 

  // Stage 2: dpath 1, pLoad, ready, pAdd, pInc, pRst

  when( global === UInt(2) || global === UInt(3) ){
    dFunc := UInt(7) // default, overwritten when global == 3
    
    when( !stall ){
      //!stall

      // turn off pRst. always one cycle after pLoad (when sCycles==0) or stall
      if( sCycles == 0 ){
        when( RegNext( pcN === UInt(n-1) ) ){ pRst := Bool(false) }
      } else{
        when( RegNext(stall) ){ pRst := Bool(false) }
      }

      pcN := pcN + UInt(1)
      pcP := pcP + UInt(1)

      pLoad := Bool(false)
      pInc := Bool(false)
      
      when( pcN === UInt( n-1 ) ){
        pLoad := Bool(true)
        pInc := Bool(true)
        ready := Bool(false)
        if( d==p ){
          pcD := pcD
        } else{
          pcD := pcD + UInt(1)
        }
        when( pcD === UInt( d/p -1 ) ){
          // move to global=3, i.e. start dpath 2
          global := UInt(3)
          dFunc := UInt(0) // mul g
          state := UInt(0)
          dAdd := Bool(true)
          if( sCycles > 0 ){
            // initiate a stall
            stall := Bool(true)
            pAdd := Bool(false)
            pRst := Bool(true)
          }
        }
      }

      when( pcN === UInt( max(0, n-p-1) ) ){
        ready := Bool(true)
      }

      when( pcN === UInt( n-1 - (n/p -1) ) ){
        when( pcD === UInt( d/p -1 ) ){
          // initiate pRst (n/p)-1 cycles before the stall, to capture all data out
          pRst := Bool(true)
        }  
      }
    }

    when( stall ){
      pLoad := Bool(false)
      pInc := Bool(false)
      sDelay := sDelay + UInt(1)
      if( sCycles > 0 ){
        when( sDelay === UInt(sCycles-1) ){
          pAdd := Bool(true)
          stall := Bool(false)
          sDelay := UInt(0)
        }
      }
    }

  }

  // Stage 3:
  val opReg = 1
  val aluReg = 3
  val iCycles = max( mStages + 1, (n/p)+1 ) + opReg //+ aluReg
  val dcI = RegInit( UInt( 0, log2Up( iCycles+3 ) ))
  val dcNP = RegInit( UInt( 0, width=log2Up( n/p ) ))
  val sc = RegInit( UInt(0, width=log2Up(sLen) ))
  
  when( global === UInt(3) ){

    when( state === UInt(0) ){
      waiting := Bool(false)
      // dFunc = 0 (mul g)
      pcNP := pcNP + UInt(1)
      dFunc := UInt(0)
      dAdd := Bool(true)
      dcI := dcI + UInt(1)

      when( pcNP === UInt(n/p -1) ){
        state := UInt(1)
        dFunc := UInt(7)
        dAdd := Bool(false)
      }
      if( n==p ){
        state := UInt(1)
        dFunc := UInt(7)
        dAdd := Bool(false)
      }

    }

    when( state === UInt(1) ){
      // dFunc = 7 (rstG)
      dFunc := UInt(7)
      dcI := dcI + UInt(1)
      when( dcI === UInt( iCycles-1+3 ) ){
        dcI := UInt(0)
        state := UInt(2)
        dFunc := UInt(3)
      }
    }
    when( state === UInt(2) ){
      // dFunc = 3 (had)
      dcD := dcD + UInt(1)
      dcHad := dcHad + UInt(1)
      dcNP := dcNP + UInt(1)

      dFunc := UInt(3)
      dAdd := Bool(false)
      when( dcD === UInt( d-2 ) ){
        dAdd := Bool(true)
      }

      if( toStall ){
        when( dcNP === UInt( n/p -1 ) ){
          // if stall - then stall
          state := UInt(7)
          dFunc := UInt(7)
        } 
      }
      

      when( dcHad === UInt( d*n/p -1 ) ){
        if( toStall ){
          state := UInt(8)
          dFunc := UInt(7)
        } else{
          state := UInt(3)
          dFunc := UInt(1)
          dAdd := Bool(true)
        }
      }
    }


    when( state === UInt(3) ){
      // dFunc = 1 ( mul s )
      pcNP := pcNP + UInt(1)
      dFunc := UInt(1)
      dAdd := Bool(true)
      when( pcNP === UInt(n/p -1) ){
        dFunc := UInt(7)
        dAdd := Bool(false)
        state := UInt(4)
      }
      if( n==p ){
        state := UInt(4)
        dFunc := UInt(7)
        dAdd := Bool(false)
      }
    }
    when( state === UInt(4) ){
      // spare
      waiting := Bool(true)
    }

    if( toStall ){

      when( state === UInt(7) ){
        // stall and return to state==2, func=3
        dFunc := UInt(7)
        sc := sc + UInt(1)
        when( sc === UInt(sLen-1) ){
          dFunc := UInt(3)
          state := UInt(2)
          sc := UInt(0)
        }
      }

      when( state === UInt(8) ){
        // stall and move to state==3, func=1, dAdd=true
        dFunc := UInt(7)
        sc := sc + UInt(1)
        when( sc === UInt(sLen-1) ){
          dFunc := UInt(1)
          state := UInt(3)
          dAdd := Bool(true)
          sc := UInt(0)
        }
      }

    }

  }


  // connect outputs and apply timing configuration

  io.xout := RegNext( xout ) //ctrl signals are latched locally
  io.yout := RegNext( yout ) // ctrl signals are latched locally
  io.rdy := ShiftRegister( ready, dStages )


  io.ctrl.prst := ShiftRegister( RegNext(pRst), dStages )
  io.ctrl.pload := ShiftRegister( pLoad, dStages )
  io.ctrl.pinc := ShiftRegister( RegNext(pInc), dStages - regOut - 1 )
  io.ctrl.padd := ShiftRegister( RegNext(pAdd), dStages - regOut )
  io.ctrl.dload := dLoad
  io.ctrl.dadd := dAdd
  io.ctrl.func := dFunc


}