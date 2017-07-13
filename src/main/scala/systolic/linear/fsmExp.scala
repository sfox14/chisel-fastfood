package linear

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
  - LFSR based phbx, where the enable=padd 
*/

class FSM( val bitWidth : Int, val fracWidth : Int, 
    val n : Int, val p : Int, val d : Int, 
    val aStages : Int, val cosFlag : Boolean = false ) extends Module{

  val io = new Bundle{

    val vld = Bool(INPUT)
    val xrdy = Bool(OUTPUT)
    val yrdy = Bool(OUTPUT)

    val ctrl = new CtrlIO()
  }

  

  val rng = new Random( 21 )

  // architecture parameters
  val regIn = 1
  val regOut = 2
  val mStages = 2*regIn + regOut //mem stages 
  val oStages = 1 // operand before alu
  //val aStages = 3 

  val hStages =  max(-1, n/p -1 - oStages - aStages) // length of hReg
  val toStall = ( hStages == -1 ) // true or false
  val sLen = max(aStages + 1 + oStages - (n/p), 0) //stall cycles

  val dStages = mStages + n/p -2 //cycles that dpath2 starts ahead of pLoad
  /*
  iCycles:
    - time (# cycles) from dFunc=0 to dFunc=3
    - to compute one multiply = oStages + aStages + 1, i.e. aStages + 2
    - to write and then read one result = mStages
    - therefore, we can begin dFunc=3 for the first dictionary after
      aStages + 2 + mStages 
    - but, we can't start dFunc=3 before all the dictionaries are processed by
      dFunc=0.  
    - therefore, max(n/p, aStages+2+mStages), i.e. iCycles >= n/p and if greater than,
      then there is a stall 
  */
  val iCycles = max(n/p, aStages + 2 + mStages) // 2 - oStages + res_out reg
  
  // number of cycles to stall
  val latency_had = d*d/(d*p/n) + sLen*d 
  val latency_phb = n*(d/p)
  var latency_alu = iCycles + latency_had + max(n/p, aStages+2)*3 + (n/p)*2 + (aStages+2)*2 + p + 3 //(aStages + 2)*6 + (n/p)*3 + 2
  if( cosFlag ){
    // ensure there is a stall for (aStages + 1) before alpha, i.e. dFunc=2
    if( toStall ){
      // add the remaining part of stall
      latency_alu = latency_alu + n/p -1
    } else{
      // add the full stall
      latency_alu = latency_alu + aStages + 1
    }
  }

  val latency_total = latency_phb + latency_alu - (n/p - 1)
  val throughput = max( latency_alu, latency_phb )
  val sCycles = max( latency_alu - latency_phb, 0 ).toInt + 1 //for the fsm transition

  println( s"Hadamard Latency: $latency_had" )
  println( s"PHBx Latency: $latency_phb")
  println( s"ALU Latency: $latency_alu")
  println( s"Total Latency: $latency_total")
  println( s"Stall: $sCycles")
  println( s"Throughput: $throughput")
  
  // state registers
  val global = RegInit( UInt(0, width=2) )
  val initial = RegInit( UInt(0, width=2) )
  val state = RegInit( UInt(0, width=4) )
  val stall = RegInit( Bool(false) )
  val waiting = RegInit( Bool(false) )

  // control signal registers (9-bit)
  val ready = RegInit( Bool(false) )
  val outReady = RegInit( Bool(false) )
  val pRst = RegInit( Bool(false) )
  val pLoad = RegInit( Bool(false) )
  //val pInc = RegInit( Bool(false) )
  val pAdd = RegInit( Bool(false) )
  val dLoad = RegInit( Bool(false) )
  val dAdd  = RegInit( Bool(false) )
  val dFunc = RegInit( UInt(0, width=4) )


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
  val dcP = RegInit( UInt(0, width=log2Up( p )) )
  val dcNP = RegInit( UInt(0, width=log2Up( n/p )) )


 
  when( global === UInt(0) ){
    // there is p data in the fifo
    when( io.vld ){

      ready := Bool(true)
      pcP := pcP + UInt(1)
      when( pcP === UInt( p-1 ) ){
        // move to compute stage
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
      //pInc := Bool(false)
      
      when( pcN === UInt( n-1 ) ){
        pLoad := Bool(true)
        //pInc := Bool(true)
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
      //pInc := Bool(false)
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

  val dcI = RegInit( UInt( 0, log2Up( iCycles+3 ) ))
  val sc = RegInit( UInt(0, width=log2Up(sLen) ))
  val sca = RegInit( UInt(0, width=log2Up(sLen + n/p)) )

  val nextState = RegInit( UInt(1, width=4) )
  
  when( global === UInt(3) ){

    when( state === UInt(0) ){
      waiting := Bool(false)
      // dFunc = 0 (mul g)
      pcNP := pcNP + UInt(1)
      dFunc := UInt(0)
      dAdd := Bool(true)
      dcI := dcI + UInt(1)

      when( pcNP === UInt(n/p -1) ){
        if( iCycles == n/p ){
          // no stall
          state := UInt(2) 
          dFunc := UInt(3) 
          //dcI := UInt(0) //reset
        } else{
          //stall
          state := UInt(1)
          dFunc := UInt(7)
        }
        dAdd := Bool(false)
      }
      if( n==p ){
        if( iCycles == n/p ){
          // no stall
          state := UInt(2)
          dFunc := UInt(3)
        }else{
          state := UInt(1)
          dFunc := UInt(7)
        }
        dAdd := Bool(false)
      }

    }

    when( state === UInt(1) ){
      // dFunc = 7 (rstG)
      dFunc := UInt(7)
      dcI := dcI + UInt(1)
      when( dcI === UInt( iCycles - 1 ) ){  
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

      // stall every n/p cycles of the hadamard
      if( toStall ){
        when( dcNP === UInt( n/p -1 ) ){
          // if stall - then stall
          state := UInt(7)
          nextState := UInt(2)
          dFunc := UInt(7)
          dAdd := Bool(false)
        } 
      }
      // stall at the end of hadamard too
      when( dcHad === UInt( d*n/p -1 ) ){
        if( toStall ){
          state := UInt(7)
          nextState := UInt(3)
          dFunc := UInt(7)
          dAdd := Bool(false)
        } else{
          state := UInt(3)
          dFunc := UInt(1)
          dAdd := Bool(true)
        }
      }
    }

    when( state === UInt(3) ){
      // dFunc = 1 ( mul s )
      dcNP := dcNP + UInt(1)
      dFunc := UInt(1)
      dAdd := Bool(true)

      when( dcNP === UInt(n/p -1) ){
        if( toStall ){
          dFunc := UInt(7)
          nextState := UInt(4)
          state := UInt(7)
          dAdd := Bool(false)
        } else{
          dFunc := UInt(4)
          //dAdd := Bool(false)
          state := UInt(4)
        }
        
      }
      if( n==p ){
        if( toStall ){
          dFunc := UInt(7)
          nextState := UInt(4)
          state := UInt(7)
          dAdd := Bool(false)
        } else{
          state := UInt(4)
          dFunc := UInt(4)
          dAdd := Bool(false)
        }
      }
    }


    when( state === UInt(4) ){
      // cosine function
      dcNP := dcNP + UInt(1)
      dFunc := UInt(4)
      dAdd := Bool(true)

      when( dcNP === UInt(n/p -1) ){
        if( toStall || cosFlag ){
          dFunc := UInt(7)
          nextState := UInt(5)
          state := UInt(7)
          dAdd := Bool(false)
        } else{
          dFunc := UInt(2)
          //dAdd := Bool(false)
          state := UInt(5)
        } 
      }
      if( n==p ){
        if( toStall || cosFlag ){
          dFunc := UInt(7)
          nextState := UInt(5)
          state := UInt(7)
          dAdd := Bool(false)
        } else{
          state := UInt(5)
          dFunc := UInt(2)
          dAdd := Bool(false)
        }
        
      }
    }

    when( state === UInt(5) ){
      // multiply alpha, dFunc=2, and sum in parallel
      dcNP := dcNP + UInt(1)
      dFunc := UInt(2)
      dAdd := Bool(true)

      when( dcNP === UInt(n/p -1) ){
        dFunc := UInt(5)
        state := UInt(6)
        dAdd := Bool(false) //no mem dependency for the sum
      }    
    }

    when( state === UInt(6) ){
      // save the local accumulated sum, move to next state
      dFunc := UInt(6)
      state := UInt(8)
    }

    when( state === UInt(8) ){
      // accumulate the PE sums
      dcP := dcP + UInt(1)
      dFunc := UInt(6)
      when( dcP === UInt(p-1) ){
        dFunc := UInt(8)
        state := UInt(9)
        dcP := UInt(0)
        outReady := Bool(true)
        //dAdd := Bool(false) //no mem dependency for the sum
      }
    }

    when( state === UInt(9) ){
      //compute the error, then stall for 5 cycles
      outReady := Bool(false)
      dFunc := UInt(7)
      dcN := dcN + UInt(1)
      when( dcN === UInt( oStages + aStages + 1 ) ){ // 1 stage prCode
        dcN := UInt(0)
        dFunc := UInt(9)
        state := UInt(10)
      }
    }

    when( state === UInt(10) ){
      // multiply err*eta, stall for 5 stages, then move to kernel mul
      dFunc := UInt(7)
      dcN := dcN + UInt(1)
      when( dcN === UInt( oStages + aStages + 1 ) ){
        dcN := UInt(0)
        dFunc := UInt(11)
        dAdd := Bool(true)
        state := UInt(11)
      }
    }

    when( state === UInt(11) ){
      
      dFunc := UInt(11)
      dAdd := Bool(true)
      dcNP := dcNP + UInt(1)
      when( dcNP === UInt( n/p-1 ) ){

        if( toStall ){
          dFunc := UInt(7)
          nextState := UInt(12)
          state := UInt(7)
          dAdd := Bool(false)
        } else{
            dFunc := UInt(10)
            //dAdd := Bool(false)
            state := UInt(12)
        } 
        if( n==p ){
          if( toStall ){
            dFunc := UInt(7)
            nextState := UInt(12)
            state := UInt(7)
            dAdd := Bool(false)
          } else{
            state := UInt(12)
            dFunc := UInt(10)
            dAdd := Bool(false)
          }
          
        }
      }

    }

    when( state === UInt(12) ){
      // update alpha
      dFunc := UInt(10)
      dAdd := Bool(true)
      dcNP := dcNP + UInt(1)
      when( dcNP === UInt( n/p -1 ) ){
        dFunc := UInt(7)
        state := UInt(14)
        dAdd := Bool(false)
      }
    }

    when( state === UInt(14) ){
      // spare
      waiting := Bool(true)
    }

    if( toStall ){

      when( state === UInt(7) ){
        // stall and return to nextState
        dFunc := UInt(7)
        sc := sc + UInt(1)
        dAdd := Bool(false)
        
        when( nextState === UInt(2) ){
          // stall to dFunc=3, hadamard
          when( sc === UInt( sLen -1 ) ){
            dFunc := UInt(3)
            sc := UInt(0)
            state := nextState
          } 
        }.elsewhen( nextState === UInt(3) ){
          // stall to dFunc=1, mul s
          when( sc === UInt( sLen -1 ) ){
            dFunc := UInt(1)
            sc := UInt(0)
            state := nextState
            dAdd := Bool(true)
            if( n==p ){
              dAdd := Bool(false)
            }
          }
        }.elsewhen( nextState === UInt(4) ){
          // stall to dFunc=4, cosine (3 pipeline registers)
          when( sc === UInt( sLen -1 ) ){
            dFunc := UInt(4)
            state := nextState
            sc := UInt(0)
            dAdd := Bool(true)
            if( n==p ){
              dAdd := Bool(false)
            }
          }
        }.elsewhen( nextState === UInt(5) ){
          // stall to dFunc=2, mul alpha
          // if cosFlag, stall for longer
          var caLen = 0
          if( toStall ){
            caLen += sLen
          }
          if( cosFlag ){
            caLen += (n/p -1)
          }
          sca := sca + UInt(1)
          sc := UInt(0)
          when( sca === UInt( caLen - 1 ) ){
            dFunc := UInt(2)
            state := nextState
            sca := UInt(0)
            dAdd := Bool(true)
            if( n==p ){
              dAdd := Bool(false)
            }    
          }
        }.elsewhen( nextState === UInt(12) ){
          // stall to dFunc=10, update alpha
          when( sc === UInt( sLen -1 ) ){
            dFunc := UInt(10)
            state := nextState
            sc := UInt(0)
            dAdd := Bool(true)
            if( n==p ){
              dAdd := Bool(false)
            }
          }
        }

      }
    }
    if( !toStall & cosFlag ){

      when( state === UInt(7) ){
        dFunc := UInt(7)
        dAdd := Bool(false)

        when( nextState === UInt(5) ){
          var caLen = aStages + 1 //n/p -1
          sca := sca + UInt(1)
          when( sca === UInt( caLen - 1 ) ){
            dFunc := UInt(2)
            state := nextState
            sca := UInt(0)
            dAdd := Bool(true)
            if( n==p ){
              dAdd := Bool(false)
            }    
          }
        }
      }

    }


  }
  println( toStall, cosFlag )


  io.xrdy := ShiftRegister( ready, dStages )
  io.yrdy := ShiftRegister( outReady, 2 + 1 + mStages + oStages + aStages ) //dFunc to aluCode

  io.ctrl.prst := ShiftRegister( RegNext(pRst), dStages )
  io.ctrl.pload := ShiftRegister( pLoad, dStages )
  io.ctrl.padd := ShiftRegister( pAdd, dStages )
  //io.ctrl.pinc := ShiftRegister( RegNext(pInc), dStages - regOut - 1 )
  //io.ctrl.padd := ShiftRegister( RegNext(pAdd), dStages - regOut )
  io.ctrl.dload := dLoad
  io.ctrl.dadd := dAdd
  io.ctrl.func := dFunc

}