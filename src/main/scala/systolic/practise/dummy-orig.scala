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

class DummyPEOrig( val id : Int, val bitWidth : Int, val fracWidth : Int,
    val n : Int, val p : Int, val d : Int, forSim : Boolean = true, 
        seed : Int = 45687 ) extends Module{
  
  val io = new Bundle{
    val xin = Fixed(INPUT, bitWidth, fracWidth)
    val xout = Fixed(OUTPUT, bitWidth, fracWidth)
    
    val hin = Fixed(INPUT, bitWidth, fracWidth)
    val hout = Fixed(OUTPUT, bitWidth, fracWidth)
    
    val ctrl = new CtrlPE().flip()
    val ctrlOut = new CtrlPE()

  }

  /*
  k = n/p  // Dictionary per PE
  h = n/d  // Hadamard blocks
  b = p/h  // PEs per Hadamard
  */
  val k = n/p
  val h = n/d
  val b = p/h

  val aStages = 2
  val mStages = 1//3
  val eStages = 1//3


  //  **** INPUT *** //
  // local pipeline register for ctrl signals
  val pload = Reg( init=Bool(false), next=io.ctrl.pload )
  val padd = Reg( init=Bool(false), next=io.ctrl.padd )
  val func = Reg( init=UInt(0, 4), next=io.ctrl.func )
  val sx = Reg( init=UInt(0, 3), next=io.ctrl.sx )
  // input x
  val x_parr = Reg( init=Fixed(0, bitWidth, fracWidth), next=io.xin )
  // input hin
  val h_parr = Reg( init=Fixed(0, bitWidth, fracWidth), next=io.hin )

  // Input Load + Parallel Input Buffer
  val x_data = ShiftRegisterEnable( x_parr, k-1, en=pload )
  val x_data2 = RegEnable( x_data, pload )
  val x_data3 = RegEnable( -x_data, pload )


  val delta = RegInit( Fixed(212, bitWidth, fracWidth) )
  

  // func shift registers
  val opCode = ShiftRegister( func, mStages + 1 )
  val aluCode = ShiftRegister( opCode, 1 + aStages )

  // application registers
  val dat_out = RegInit( Fixed(0, bitWidth, fracWidth) )

  // PE memory
  val porig = ShiftRegister( dat_out, k - aStages - eStages - 1) //-3) 
  val preg = ShiftRegister( porig, 3 )
  val pplus = RegNext( preg )
  val pminus = RegNext( -preg )


  val hin = Fixed(width=bitWidth, fracWidth=fracWidth)
  val hSel = Bool()
  hSel := ( sx <= UInt(log2Up(b)-1, 3) )
  hin := Mux( hSel, io.hin, dat_out )


  val ram = (0 until 8*k).map( x => BigInt(0) ).toVector
  val dataMem = Module( new PipelinedDualPortLutRAM( Fixed(width=bitWidth, fracWidth=fracWidth),
                                        log2Up(8*k), 1, 1, id, ram, forSim ) )


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
  dataMem.io.ports(0).req.addr := rdAddr
  val hreg = dataMem.io.ports(0).rsp.readData

  // port 1 - write
  dataMem.io.ports(1).req.addr := wrAddr 
  dataMem.io.ports(1).req.writeData := hin
  dataMem.io.ports(1).req.writeEn := write



  // operand for bx, random samples from LFSR
  val randb = Module( new LFSR( 45687 ) ) // 45687 ) )  // 
  randb.io.en := RegNext( pload )
  val index = randb.io.out
  val op_bx = RegInit( Fixed(0, bitWidth, fracWidth) )
  op_bx := x_data2
  when( !index ){
    op_bx := x_data3
  }

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
        ( opCode === UInt(13) ) -> delta,
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

  def subtractStage( op1 : Fixed, op2 : Fixed ): Fixed = {
    val a = RegNext( op1 )
    val b = RegNext( op2 )
    val out = ShiftRegister( (a - b), aStages - 1 )
    out
  }

  val mul = Module( new MulMod( bitWidth, fracWidth, 1) )
  mul.io.op1 := op1
  mul.io.op2 := op2

  val alu_out = Fixed(width=bitWidth, fracWidth=fracWidth)
  alu_out := MuxCase( Fixed(0, bitWidth, fracWidth), Array(
        ( aluCode === UInt(0) || 
          aluCode === UInt(1) ||
          aluCode === UInt(12)) -> adderStage( op1, op2 ),
        ( aluCode === UInt(4) ) -> subtractStage( op1, op2 ),
        ( aluCode === UInt(2) || 
          aluCode === UInt(3) ||
          aluCode === UInt(13) ||
          aluCode === UInt(8) || 
          aluCode === UInt(5) ) -> mul.io.res  
                    ))

  dat_out := alu_out
  io.xout := x_parr
  io.hout := dat_out


  io.ctrlOut := RegNext( io.ctrl )
  
}




