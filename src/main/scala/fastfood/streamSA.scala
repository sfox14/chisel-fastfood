package fastfood

import Chisel._
import com.github.tototoshi.csv._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.util.Random

import utils._

import math._
/*
Stream Systolic Array
----------------------
*/

class ControlUnit( val d : Int, val p : Int, val loops : Int ) extends Module{
  val io = new Bundle{
    val inValid = Bool( INPUT )
    val stall = Bool( OUTPUT )
    val outValid = Bool( OUTPUT )
    val ringValid = Bool( OUTPUT )
    val outAddr = UInt( OUTPUT, width=log2Up(p+1) )
  }

  val cin = RegInit( UInt(0, log2Up( d ) ) )
  val cout = RegInit( UInt(0, log2Up( p ) ) )
  val cloop = RegInit( UInt( 0, log2Up( loops ) ) )

  // next inValid is the last feature
  val sof = Bool()
  val eof = Bool()
  val outValid = RegInit( Bool(false) )

  sof := Bool(false)
  eof := Bool(false)

  when( io.inValid ){
    when( cin === UInt(d-1, log2Up( d ) ) ){
      cin := UInt(0)
      sof := Bool(true) //start propagating outValid
      
      if ( loops > 1){
        cloop := cloop + UInt(1)
        when( cloop === UInt( loops-1, log2Up( loops ) ) ){
          cloop := UInt(0) 
        }
      }

    }.otherwise{
      cin := cin + UInt(1,1)
    }
  }

  when( eof ){
    outValid := Bool(false)
  }

  // sof && eof, sof will override eof and outValid = true
  when( sof ){
    outValid := Bool(true)
  }


  when( outValid ){
    when( (cout === UInt( p-1, log2Up( p ) ) ) ){
      cout := UInt(0)
      eof := Bool(true)
    }.otherwise{
      cout := cout + UInt(1,1)
    }
  }

  /*
  ringValid - propagated through PE's to determine which inputs are invalid 
              during loop back. the signal goes low for the first valid input
              of the final loop.

  outValid  - determines when a PE is valid. when the last feature in an input
              sequence is received at the first PE, an outValid is generated
              which is propgated through the array for p PEs. 

  stall     - stalls the input stream during loop back.

  outAddr   - address of the PE which currently has an outValid. this
              implementation uses a massive p-length multiplexor to select
              the module's output. this is decoupled from the ring array, and
              can be pipelined.

  */

  io.ringValid := !(( cloop === UInt( loops-1, log2Up( loops ) ) ) )
  io.outValid := outValid //&& (cout === UInt(0) )
  io.stall := !( cloop === UInt(0) )
  io.outAddr := cout
  
}


/*
Top module:
  - input buffer >> systolic array >> output buffer
*/

class StreamTop( val n_dicts : Int, val n_features : Int, val bitWidth : Int, 
  val fracWidth : Int, val weights : List[List[Int]], val a : Int, val p : Int,
  val col : Int, val s : Int, fifoDepth : Int = 100) extends Module {

  val io = new Bundle{
    val dIn = Decoupled( Fixed( INPUT, bitWidth, fracWidth ) ).flip
    val dOut = Valid( Fixed( OUTPUT, bitWidth, fracWidth ) )
  }

  val sysArray = Module( new StreamSA( n_dicts, n_features, bitWidth, fracWidth,
                                  weights, a, p, col, s))

  val inFifo = Module( new Queue( Fixed( width = bitWidth, fracWidth = fracWidth ), fifoDepth ) )

  inFifo.io.enq <> io.dIn
  sysArray.io.dIn <> inFifo.io.deq

  val outFifo = Module( new Queue( Fixed( width = bitWidth, fracWidth = fracWidth ), fifoDepth ) )

  outFifo.io.enq.bits := sysArray.io.dOut.bits
  outFifo.io.enq.valid := sysArray.io.dOut.valid
  
  outFifo.io.deq.ready := Bool(true)
  io.dOut.bits := outFifo.io.deq.bits
  io.dOut.valid := outFifo.io.deq.valid

}


/*
Streaming systolic array.
*/

class StreamSA( val n_dicts : Int, val n_features : Int, val bitWidth : Int, 
  val fracWidth : Int, val weights : List[List[Int]], val a : Int, val p : Int,
  val col : Int, val s : Int) extends Module {


  val io = new Bundle{
    val dIn = Decoupled( Fixed( INPUT, bitWidth, fracWidth ) ).flip
    val dOut = Valid( Fixed( OUTPUT, bitWidth, fracWidth ) ) 
  }

    // number of loops
  val loops = n_dicts/(a*col*p)
  // number of input cycles
  val d = math.ceil( n_features/s.toDouble ).toInt

  // few architecture requirements
  Predef.assert( n_dicts%(a*col*p) == 0, "Error: number of parallel units not a factor of n_dicts")
  Predef.assert( d>=p, "Error: too many processing elements, p>d")

 
  val ssarray = (0 until p).map( x => Module( new StreamPE(n_features, bitWidth,
                                                                fracWidth, weights(x),
                                                                col, s ) ) )

  // control module
  val ctrl = Module( new ControlUnit(d, p, loops) )

  // loop back fifo
  val ringFifo = Module( new Queue( Fixed( width = bitWidth, fracWidth = fracWidth ), 
                                        math.max( 20, 1 ) ) )

  val inData = Fixed( width=bitWidth, fracWidth=fracWidth )
  val inValid = Bool()

  // default connections
  inData := io.dIn.bits
  inValid := io.dIn.valid
  io.dIn.ready := Bool(true)

  // use a fifo on the loop back path to regulate data
  if ( loops>1 ){
    ringFifo.io.enq.bits := ssarray(p-2).io.dPass.bits
    ringFifo.io.enq.valid := ssarray(p-2).io.dPass.valid && ssarray(p-2).io.ringOutValid
    ringFifo.io.deq.ready := ctrl.io.stall || (!ctrl.io.stall && !ringFifo.io.deq.valid)

    when( ctrl.io.stall ){
      //inData := Mux( !ctrl.io.stall, io.dIn.bits, ringFifo.io.deq.bits )
      //inValid := Mux( !ctrl.io.stall, io.dIn.valid, ringFifo.io.deq.valid )
      inData := ringFifo.io.deq.bits
      inValid := ringFifo.io.deq.valid
    }
    
    io.dIn.ready := !ctrl.io.stall
  }

  // connect input to control unit
  ctrl.io.inValid := inValid

  // connect array as a sequence of processing elements
  ssarray(0).io.dIn.bits := inData
  ssarray(0).io.dIn.valid := inValid
  ssarray(0).io.ringValid := ctrl.io.ringValid
  ssarray(0).io.outValid := ctrl.io.outValid && ( ctrl.io.outAddr === UInt(0) )
  for( ix <- 1 until p ){
    ssarray(ix).io.dIn <> ssarray(ix-1).io.dPass
    ssarray(ix).io.outValid := ssarray(ix-1).io.nextOutValid
    ssarray(ix).io.ringValid := ssarray(ix-1).io.ringOutValid
  }
  

  val index = ctrl.io.outAddr
  val default = ssarray(0).io.dOut.bits

  // mux determines which output/s are valid each cycle
  io.dOut.bits := MuxLookup( index, default, (0 until p).map( x => ( UInt( x, width=log2Up(p+1) ),
                                                            ssarray(x).io.dOut.bits ) ) )

  io.dOut.valid := ctrl.io.outValid

}


  /*
  // connect the outputs
  def reduceNums( numsToSum : Seq[Fixed] ) : ( Fixed, Int ) = {
    var stages = 0
    var tmpNums = numsToSum
    while( tmpNums.size > 1 ) {
      tmpNums = tmpNums.grouped(3).map( x => RegNext( x.reduce( _ + _ ) ) ).toSeq
      stages += 1
    }
    ( tmpNums(0), stages )
  }

  val outNums = (0 until p).map( x => ssarray(x).io.dOut )
  val (adder, stages ) = reduceNums( outNums )
  */