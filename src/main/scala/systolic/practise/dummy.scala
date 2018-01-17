package practise

import Chisel._
import com.github.tototoshi.csv._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.util.Random

import utils._
import xilinx._

import math._


class MulModIO(bitWidth : Int, fracWidth : Int) extends Bundle{
  val op1 = Fixed(INPUT, bitWidth, fracWidth)
  val op2 = Fixed(INPUT, bitWidth, fracWidth)
  val res = Fixed(OUTPUT, bitWidth, fracWidth)

  def setNames(){
    op1.setName( "A" )
    op2.setName( "B" )
    res.setName( "P" )
  }
}

class MulModBB(bitWidth : Int, fracWidth : Int) extends BlackBox{
  val io = new MulModIO(bitWidth, fracWidth)
  io.setNames()

  addClock( Driver.implicitClock )
  renameClock( Driver.implicitClock, "CLK" )
  setModuleName("mult_gen_0")

  //simulate
  io.res := (io.op1 * io.op2)

}

class MulMod( bitWidth : Int, fracWidth : Int, nreg : Int) extends Module {
  val io = new MulModIO(bitWidth, fracWidth)

  val mul = Module( new MulModBB( bitWidth, fracWidth ) )

  mul.io.op1 := ShiftRegister( io.op1, nreg )
  mul.io.op2 := ShiftRegister( io.op2, nreg )
  io.res := ShiftRegister( mul.io.res, nreg ) 

}


class CtrlPE extends Bundle {
  // PE control signals
  val pload = Bool(OUTPUT)
  val yload = Bool(OUTPUT)
  val padd = Bool(OUTPUT)
  val func = UInt(OUTPUT, 4)
  val sx = UInt(OUTPUT, 3)

}

class CtrlIO extends Bundle{

  val pe = new CtrlPE

  override def clone = { new CtrlIO().asInstanceOf[this.type] }

}

class LFSR( val init : Int ) extends Module {
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

object ShiftRegisterEnable {
  /** @param in input to delay
    * @param n number of cycles to delay
    * @param en enable the shift */
  def apply[T <: Data](in: T, n: Int, en: Bool) : T =
  {
    // The order of tests reflects the expected use cases.
    if (n == 1) {
      RegEnable(in, en)
    } else if (n != 0) {
      RegEnable(apply(in, n-1, en), en)
    } else {
      in
    }
  }
}

// PE version with Fast Hadamard Transform

class DummyPE( val id : Int, val bitWidth : Int, val fracWidth : Int,
    val n : Int, val p : Int, val d : Int, forSim : Boolean = true, 
        seed : Int = 45687 ) extends Module {
  
  val io = new Bundle{
    val xin = Fixed(INPUT, bitWidth, fracWidth)
    val xout = Fixed(OUTPUT, bitWidth, fracWidth)
    
    val hin = Fixed(INPUT, bitWidth, fracWidth)
    val hout = Fixed(OUTPUT, bitWidth, fracWidth)

    val kin = Fixed(INPUT, bitWidth, fracWidth)
    
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
  // input kin
  val kin = Reg( init=Fixed(0, bitWidth, fracWidth), next=io.kin )


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
  val preg = RegNext( porig )
  val pplus = RegNext( preg )
  val pminus = RegNext( -preg )


  val hin = RegInit( Fixed(0, width=bitWidth, fracWidth=fracWidth) )
  val hSel = Bool()
  hSel := ( sx <= UInt(log2Up(b)-1, 3) )
  hin := Mux( hSel, h_parr, dat_out )
  /*hin := MuxCase( dat_out, Array( 
          ( hSel === UInt(1) ) -> h_parr,
          ( hSel === UInt(2) ) -> kin
                ))
  */

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
        ( opCode === UInt(6) ) -> kin,
        ( opCode === UInt(8) ) -> op_bx,
        ( opCode === UInt(2) ) -> delta,
        ( opCode === UInt(9) ) -> Fixed(0, bitWidth, fracWidth),
        ( opCode === UInt(5) ) -> Fixed( toFixed(1.0, fracWidth), bitWidth, fracWidth )
                ))

  // select operand 2
  val op2 = RegInit( Fixed(0, bitWidth, fracWidth) )
  op2 := MuxCase( RegNext( hreg ), Array(               
        ( opCode === UInt(6) ||
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
          aluCode === UInt(12)) -> adderStage( op1, op2 ),
        ( aluCode === UInt(4) ) -> subtractStage( op1, op2 ),
        ( aluCode === UInt(2) || 
          aluCode === UInt(5) ) -> RegNext( mul.io.res )  
                    ))
  dat_out := alu_out
  

  io.xout := x_parr
  io.hout := dat_out
  io.ctrlOut := RegNext( io.ctrl )
  
}



class SignGen( val id : Int, k : Int, b : Int ) extends Module{
  val io = new Bundle{
    val func = UInt(INPUT, width=4)
    val counter = UInt( INPUT, width=log2Up(k) )
    val lvl = UInt( INPUT, width=log2Up(k/2)+1 )
    val out = Bool(OUTPUT)
  }

  // decode ctrl signal
  val incr = ( io.counter === UInt(k-1) )
  val hcg = ( io.counter & io.lvl )

  def parity( x : Int ) : Boolean = {
    var k = 0
    var d = x
    while( d != 0 ){
      k = k + 1
      d = d & (d-1)
    }
    ( k%2 == 0)
  }

  val a = ( 0 until b ).map( x => (0 until 2 ).reverse.map( y => parity( x & ( pow(2,y).toInt ) ) ) )

  val s0 = a( id ).map( x => RegInit( Bool( x ) ) )
  val s1 = !xorR( hcg ) // xor-reduce
  

  when( io.func === UInt(0) && incr ){
    s0(0) := s0(1)
    for( ix <- 1 until s0.length ){
      s0(ix) := s0(ix-1)
    }
  }
  
  io.out := MuxCase( Bool(true), Array(
        ( io.func === UInt(0) ) -> s0.last, 
        ( io.func === UInt(1) ) -> s1
               ))
}

class AddrGen( k : Int, bitWidth : Int, fracWidth : Int ) extends Module{

  val io = new Bundle{
    val counter = UInt(INPUT, width=log2Up(k) )
    val func = UInt(INPUT, width=4)
    val porig = Fixed(INPUT, bitWidth, fracWidth)
    val rdAddr = UInt(OUTPUT, width=11 ) //11 is the width req for k=256
    val wrAddr = UInt(OUTPUT, width=11 )
    val lvl = UInt(OUTPUT, width=log2Up(k/2)+1 )
  }

  val toggle = RegInit( UInt(0, width=1) )
  val lvl = RegInit( UInt(1, width= log2Up(k/2)+1 ) )  //max 5 bits for k=64

  //decode ctrl signal
  val active = ( io.func === UInt(1, 4) && io.counter === UInt( k-1 ) )
  val end = ( io.func =/= UInt(1, 4) )

  when( active ){
    lvl := lvl<<UInt(1)
  }
  when( end ){
    lvl := UInt(1)
    toggle := UInt(0)
  }

  // address from hadamard
  val hadGen = UInt( width=log2Up(k) )
  hadGen := io.counter ^ lvl // bitwise XOR

  //toggle the memory location of the hadamard
  when( active ){
    toggle := !toggle
  }

     // Cosine table parameters
  val intWidth = (bitWidth - fracWidth)
  val lutWidth = log2Up( 256 ) // 2**(lutWidth) = lutSize entries
  val lutFracWidth = lutWidth - 1 // only one bit for integer (mod 2)
  val cosAddr = io.porig( (bitWidth - intWidth) , (fracWidth - lutFracWidth) )

  // Default for k=32
  var rCos = UInt(1,2) ## cosAddr
  var rDefault = UInt(0,2) ## io.func(2,0) ## io.counter
  var rHad = UInt(0,2) ## UInt(0,1) ## toggle ## hadGen

  val rdAddr = UInt(width=11)
  rdAddr := MuxCase( rDefault, Array(
                    ( io.func === UInt(1, 4) ) -> rHad,
                    ( io.func === UInt(5, 4) ) -> rCos ))  

  // break path up, add a register here, and reduce delay by 1
  val wToggle = RegNext(!toggle)
  val wHadGen = RegNext( io.counter )
  val wDefault = RegNext( rdAddr )
  val wSel1 = RegNext( (io.func === UInt(1,4)) )
  val wSel2 = RegNext( (io.func === UInt(2,4)) ) // write to addr 0
  val wSel3 = RegNext( (io.func === UInt(5,4)) ) // write back for Cosine

  // hard code here for k=32
  val wHad = ( UInt(0, 2) ## UInt(0, 2) ## wToggle ## wHadGen )
  val wrAddr = UInt(width=11)
  wrAddr := MuxCase( wDefault, Array( 
                ( wSel1 ) -> wHad, 
                ( wSel2 ) -> ( UInt(0,2) ## UInt(0, 3) ## wHadGen ),
                ( wSel3 ) -> ( UInt(0,2) ## UInt(5, 3) ## wHadGen ) 
                    ))

  // connect outputs
  io.rdAddr := rdAddr
  io.wrAddr := wrAddr // *** includes a delay of 1
  io.lvl := lvl

}