package practise

import Chisel._
import com.github.tototoshi.csv._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.util.Random

import utils._
import xilinx._

import math._


// PE version with Fast Hadamard Transform

class OocPE( val id : Int, val bitWidth : Int, val fracWidth : Int,
    val n : Int, val p : Int, val d : Int, g : Seq[BigInt], s : Seq[BigInt], 
      alpha : Seq[BigInt], val aStages : Int, forSim : Boolean = true, 
        seed : Int = 45687 ) extends Module{
  
  val io = new Bundle{
    val xin = Fixed(INPUT, bitWidth, fracWidth)
    val xout = Fixed(OUTPUT, bitWidth, fracWidth)
    
    val hin = Vec.fill( log2Up( p*d/n ) ){ Fixed(INPUT, bitWidth, fracWidth) }
    val hout = Fixed(OUTPUT, bitWidth, fracWidth)
    
    val delta = Fixed(INPUT, bitWidth, fracWidth)
    val deltaOut = Fixed(OUTPUT, bitWidth, fracWidth)
    
    val ctrl = new CtrlPE().flip()
    val ctrlOut = new CtrlPE()

    val a_addr = UInt(OUTPUT, 10)
    val a_din = Fixed(OUTPUT, bitWidth, fracWidth)
    val a_wr = Bool(OUTPUT)
    val a_dout = Fixed(INPUT, bitWidth, fracWidth)

    val b_addr = UInt(OUTPUT, 10)
    val b_din = Fixed(OUTPUT, bitWidth, fracWidth)
    val b_wr = Bool(OUTPUT)
    val b_dout = Fixed(INPUT, bitWidth, fracWidth)

    val dsp_a = Fixed(OUTPUT, bitWidth, fracWidth)
    val dsp_b = Fixed(OUTPUT, bitWidth, fracWidth)
    val dsp_p = Fixed(INPUT, bitWidth, fracWidth)

  }

  /*
  k = n/p  // Dictionary per PE
  h = n/d  // Hadamard blocks
  b = p/h  // PEs per Hadamard
  */

  // 512 x bitWidth ROMs for Cosine LUT
  def cosTable(A : Double, b: Double, n : Int = 512) = {

    val tab = (0 until n).map(x => x/(1<<(log2Up(n)-1) ).toDouble )
              .map(y => A*cos(y*Pi + b) )
    val fixedTab = tab.map( x => toFixed(x, fracWidth) ).toVector
    fixedTab
  }

  // local pipeline register for ctrl signals
  val pload = Reg( init=Bool(false), next=io.ctrl.pload )
  val padd = Reg( init=Bool(false), next=io.ctrl.padd )
  val func = Reg( init=UInt(0, 4), next=io.ctrl.func )
  val sx = Reg( init=UInt(0, 3), next=io.ctrl.sx )

  io.ctrlOut := RegNext( io.ctrl )
  io.deltaOut := RegNext( io.delta )

  val k = n/p
  val h = n/d
  val b = p/h

  val mStages = 4
  val eStages = 3

  Predef.assert( ( k/2 ) >= aStages + mStages + eStages, "Error: Maximum switch, k/2, greater than the number of pipeline stages in datapath" )

  // input load
  val x_parr = Reg( init=Fixed(0, bitWidth, fracWidth), next=io.xin )
  val x_data = ShiftRegisterEnable( x_parr, k-1, en=pload )
  val x_data2 = RegEnable( x_data, pload )
  val x_data3 = RegEnable( -x_data, pload )

  // func shift registers
  val opCode = ShiftRegister( func, mStages + 1 )
  val aluCode = ShiftRegister( opCode, 1 + aStages )

  // application registers
  val dat_out = RegInit( Fixed(0, bitWidth, fracWidth) )

  // PE memory
  val porig = ShiftRegister( dat_out, k - aStages - eStages - 1 - 3)
  //val porig = Module( new BramSreg( Fixed(width=bitWidth, fracWidth=fracWidth), 
  //							k - aStages - eStages - 3,
  //							aStages + eStages, forSim) )
  //porig.io.in := dat_out
  //val preg = ShiftRegister( porig.io.out, 3 ) // porig.io.out
  val preg = ShiftRegister( porig, 3 )
  val pplus = RegNext( preg )
  val pminus = RegNext( -preg )


  val hin = Fixed(width=bitWidth, fracWidth=fracWidth)
  val hSel = Bool()
  hSel := ( sx <= UInt(log2Up(b)-1, 3) )
  hin := Mux( hSel, io.hin(sx), dat_out )

  val rng9 = new Random(11)
  var amp = sqrt(2.0/n)
  var u = rng9.nextDouble()*2*Pi
  val cosine = cosTable( amp, u, 256 )
  val kern = (0 until k).map( x => BigInt(0) ).toVector
  val extra = (0 until 2*k).map( x => BigInt(0) ).toVector
  val extra256 = (0 until k).map( x => BigInt(0) ).toVector
  val had = (0 until 2*k).map( x => BigInt(0) ).toVector

  var ram32 = had ++ g ++ s ++ alpha ++ kern ++ extra ++ cosine ++ extra
  var ram64 = had ++ g ++ s ++ alpha ++ kern ++ extra ++ cosine
  var ram128 = had ++ g ++ s ++ alpha ++ kern ++ cosine
  var ram256 = had ++ g ++ s ++ alpha ++ kern ++ cosine ++ extra256

  var mem = ram32
  var aw = 10
  if( k==64 ){ mem = ram64 }
  else if( k==128 ){ mem = ram128 }
  else if( k==256 ){ 
    mem = ram256 
    aw = 11
  }

  //val dataMem = Module( new PipelinedDualPortBRAM( Fixed(width=bitWidth, fracWidth=fracWidth),
  //                                      aw, 1, 2, id, mem, forSim ) ) 

  // PE data counter
  val counter = RegInit( UInt(0, log2Up(k)) )
  when( padd ){
    counter := counter + UInt(1)
  }
  val counterA = ShiftRegister( counter, 2 ) // 2 cycles when BRAM used


  // Address Generator
  val agen = Module( new AddrGen( k, bitWidth, fracWidth ) )
  agen.io.counter := counterA
  agen.io.func := func
  agen.io.porig := porig
  val rdAddr = agen.io.rdAddr
  val wrAddr = ShiftRegister( agen.io.wrAddr, mStages + aStages + eStages - 1 ) // delay, -1, pipeline stage in AddrGen

  // write enable
  val write = ShiftRegister( (  opCode === UInt(8, 4) || 
                                opCode === UInt(0, 4) ||
                                opCode === UInt(1, 4) ||
                                opCode === UInt(2, 4) ||
                                opCode === UInt(5, 4) ||
                                opCode === UInt(12, 4)  
                              ), eStages-1 + aStages )


  // port0 - read
  val awr = RegInit( Bool(false) )
  val adin = RegInit( Fixed(0, bitWidth, fracWidth) )
  io.a_wr := awr
  io.a_addr := RegNext(rdAddr)
  io.a_din := adin
  val hreg = RegInit( Fixed(0, bitWidth, fracWidth) )
  hreg := io.a_dout

  // port 1 - write
  io.b_addr := RegNext(wrAddr) 
  io.b_din := RegNext(hin)
  io.b_wr := write
  val rbreg = Reg(init=Fixed(0, bitWidth, fracWidth), next=io.b_dout) 


  //var pid = id
  //var sd = (35605*(( ((b*hid) + pid) )/p)).toInt

  // operand for bx, random samples from LFSR
  val randb = Module( new LFSR( 45687 ) ) // 45687 ) )  // 
  randb.io.en := RegNext( pload )
  val index = randb.io.out
  val op_bx = RegInit( Fixed(0, bitWidth, fracWidth) )
  op_bx := x_data2
  when( !index ){
    op_bx := x_data3
  }

  println(id, k, b)
  // sign of preg operand
  val sign = Module( new SignGen( id, k, b ) )
  sign.io.func := func
  sign.io.counter := counterA
  sign.io.lvl := agen.io.lvl
  val pSel = Bool()
  pSel := ShiftRegister( sign.io.out, mStages  )


  // select operand 1
  val psign = RegInit( Fixed(0, bitWidth, fracWidth) )
  psign := Mux( pSel, pplus, pminus )

  val op1 = RegInit( Fixed(0, bitWidth, fracWidth) )
  op1 := MuxCase( psign, Array( 
        ( opCode === UInt(8) ) -> op_bx,
        ( opCode === UInt(13) ) -> io.delta,
        ( opCode === UInt(15) ||
          opCode === UInt(14) ||
          opCode === UInt(7) ||
          opCode === UInt(6) ||
          opCode === UInt(9) ) -> Fixed(0, bitWidth, fracWidth),
        ( opCode === UInt(5) ) -> Fixed( toFixed(1.0, fracWidth), bitWidth, fracWidth )
                ))


  // select operand 2
  val op2 = RegInit( Fixed(0, bitWidth, fracWidth) )
  op2 := MuxCase( RegNext( hreg ), Array(               
        ( opCode === UInt(15) ||
          opCode === UInt(14) ||
          opCode === UInt(7) ||
          opCode === UInt(6) ||
          opCode === UInt(9) ) -> Fixed(0, bitWidth, fracWidth),
        ( opCode === UInt(8) ) -> Fixed(toFixed(1.0, fracWidth), bitWidth, fracWidth)  
                ))
  
  // alu operations
  // 1. adder
  def adderStage( op1 : Fixed, op2 : Fixed ): Fixed = {
    val a = RegNext( op1 )
    val b = RegNext( op2 )
    val out = ShiftRegister( (a + b), aStages - 1 )
    out
  }

  /*
  // 2. Pipelined DSP multiply
  def dspMultiply( op1 : Fixed, op2 : Fixed, regIn : Int, regOut : Int): Fixed = {
    val a = ShiftRegister( op1.toSInt, regIn )
    val b = ShiftRegister( op2.toSInt, regIn )
    val out = ShiftRegister( a * b, aStages - regIn ).toUInt
    convToFixed( out )
  }

  def convToFixed( a : UInt ) : Fixed = {
    val b = (a>>fracWidth)
    val fixedType = Fixed(width=bitWidth, fracWidth=fracWidth)
    fixedType.fromBits( (0 until bitWidth).reverse.map( x => b(x) ).reduce(_##_) )
  }
  */

  io.dsp_a := op1
  io.dsp_b := op2

  val alu_out = Fixed(width=bitWidth, fracWidth=fracWidth)
  alu_out := MuxCase( Fixed(0, bitWidth, fracWidth), Array(
        ( aluCode === UInt(0) || 
          aluCode === UInt(1) ||
          aluCode === UInt(12 )) -> adderStage( op1, op2 ),
        ( aluCode === UInt(2) || 
          aluCode === UInt(3) ||
          aluCode === UInt(4) ||
          aluCode === UInt(13) ||
          aluCode === UInt(8) || 
          aluCode === UInt(5) ) -> io.dsp_p  
                    ))

  dat_out := alu_out
  io.xout := x_parr
  io.hout := dat_out
  
}
