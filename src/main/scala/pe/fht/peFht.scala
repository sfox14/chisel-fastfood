package fht

import Chisel._
import com.github.tototoshi.csv._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.util.Random

import utils._

import math._


object ShiftRegisterEnable{
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
//  - not working for N==P

class PEfht( val id : Int, val bitWidth : Int, val fracWidth : Int,
    val n : Int, val p : Int, val d : Int, g : Seq[BigInt], s : Seq[BigInt], 
      alpha : Seq[BigInt], val aStages : Int, forSim : Boolean = true, 
        seed : Int = 45687 ) extends Module{
  
  val io = new Bundle{
    val xin = Fixed(INPUT, bitWidth, fracWidth)
    val hin = Vec.fill( log2Up( p*d/n ) ){ Fixed(INPUT, bitWidth, fracWidth) }
    val delta = Fixed(INPUT, bitWidth, fracWidth)
    
    val xout = Fixed(OUTPUT, bitWidth, fracWidth)
    val hout = Fixed(OUTPUT, bitWidth, fracWidth)
    val sout = Fixed(OUTPUT, bitWidth, fracWidth)

    val ctrl = new CtrlPE().flip()

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

  //println(s"PE_$id: ", ram)


  // local pipeline register for ctrl signals
  val pload = Reg( init=Bool(false), next=io.ctrl.pload )
  val padd = Reg( init=Bool(false), next=io.ctrl.padd )
  val func = Reg( init=UInt(0, 4), next=io.ctrl.func )
  val sx = Reg( init=UInt(0, 3), next=io.ctrl.sx )


  val k = n/p
  val h = n/d
  val b = p/h

  val mStages = 4 //4 //4
  val eStages = 3 // extra = dat_out + op1/op2 + other pipeline registers

   // Cosine table parameters
  val intWidth = (bitWidth - fracWidth)
  val lutWidth = log2Up( 512 ) // 2**(lutWidth) = lutSize entries
  val lutFracWidth = lutWidth - 1 // only one bit for integer (mod 2)
  
  Predef.assert( ( k/2 ) >= aStages + mStages + eStages, "Error: Maximum switch, k/2, greater than the number of pipeline stages in datapath" )

  // input load
  val x_parr = Reg( init=Fixed(0, bitWidth, fracWidth), next=io.xin )
  val x_data = ShiftRegisterEnable( x_parr, k, en=pload ) // ShiftEnableRegister

  // func shift registers
  val opCode = ShiftRegister( func, mStages + 1 ) // extra pipeline stage between preg and op1
  val aluCode = ShiftRegister( opCode, 1 + aStages )

  // application registers
  val dat_out = RegInit( Fixed(0, bitWidth, fracWidth) )
  val sum_local = RegInit( Fixed(0, bitWidth, fracWidth ) )


  // PE memory
  val preg = ShiftRegister( dat_out, k - aStages - eStages )

  val hin = Fixed(width=bitWidth, fracWidth=fracWidth)
  val hSel = Bool()
  hSel := ( sx === UInt(0, 3) || sx === UInt(1, 3) )
  if( b == 8 ){
    hSel := ( sx === UInt(0, 3) || sx === UInt(1, 3) || sx === UInt(2, 3) )
  } else if( b == 16 ){
    hSel := ( sx === UInt(0, 3) || sx === UInt(1, 3) || sx === UInt(2, 3) || sx === UInt(3, 3) )
  }
  hin := Mux( hSel, io.hin(sx), dat_out ) //, //*note: changes in sx should be aligned with dat_out


  val cosine = cosTable( 1.0, 0, 512 )
  val kern = (0 until k).map( x => BigInt(0) ).toVector
  val extra = (0 until 2*k).map( x => BigInt(0) ).toVector
  val had = (0 until 2*k).map( x => BigInt(0) ).toVector
  var ram = had ++ g ++ s ++ alpha ++ kern
  //val ram = (0 until 8*k).map( x => BigInt(0) )
  /*val dataMem = Module( new DualPortBRAM( Fixed(width=bitWidth, fracWidth=fracWidth),
                                          log2Up(8*k), id, ram, forSim ) )*/

  ///*
  val dataMem = Module( new PipelinedDualPortBRAM( Fixed(width=bitWidth, fracWidth=fracWidth),
                                          log2Up(8*k), 1, 2, id, ram, forSim ) )
  //*/

  // PE data counter
  val counter = RegInit( UInt(0, log2Up(k)) )
  when( padd ){
    counter := counter + UInt(1)
  }

  // Address Generator
  val agen = Module( new AddrGen( k ) )
  agen.io.counter := counter
  agen.io.func := func
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
  val randb = Module( new LFSR( 35605 ) ) // 45687 ) )  // 
  randb.io.en := RegNext( pload )  //***
  val index = randb.io.out
  val op_bx = RegInit( Fixed(0, bitWidth, fracWidth) ) //Fixed( width=bitWidth, fracWidth=fracWidth )
  op_bx := x_data
  when( !index ){
    op_bx := x_data //-x_data ***************************************************
  }

  // sign of preg operand
  val sign = Module( new SignGen( id, k, b ) )
  sign.io.func := func
  sign.io.counter := counter
  sign.io.lvl := agen.io.lvl
  val pSel = Bool()
  pSel := ShiftRegister( sign.io.out, mStages  )


  // select operand 1
  val psign = RegInit( Fixed(0, bitWidth, fracWidth) ) //Fixed(width=bitWidth, fracWidth=fracWidth)
  psign := Mux( pSel, preg, -preg )

  val op1 = RegInit( Fixed(0, bitWidth, fracWidth) )
  op1 := MuxCase( psign, Array( 
        ( opCode === UInt(8) ) -> op_bx,
        ( opCode === UInt(13) ) -> io.delta,
        ( opCode === UInt(15) ||
          opCode === UInt(14) ||
          opCode === UInt(7) ||
          opCode === UInt(6) ||
          opCode === UInt(9) ) -> Fixed(0, bitWidth, fracWidth)
                ))



  // select operand 2
  val op2 = RegInit( Fixed(0, bitWidth, fracWidth) )
  op2 := MuxCase( RegNext( hreg ), Array(                //RegNext - match extra pipeline stage
        ( opCode === UInt(15) ||
          opCode === UInt(14) ||
          opCode === UInt(7) ||
          opCode === UInt(6) ||
          opCode === UInt(9) ||
          opCode === UInt(5) ) -> Fixed(0, bitWidth, fracWidth)  
                ))
  

  // alu operations

  def adderStage( op1 : Fixed, op2 : Fixed ): Fixed = {
    val a = RegNext( op1 )
    val b = RegNext( op2 )
    val out = ShiftRegister( (a + b), aStages - 1 )
    out
  }

  def convToFixed( a : UInt ) : Fixed = {
    val b = (a>>fracWidth)
    val fixedType = Fixed(width=bitWidth, fracWidth=fracWidth)
    fixedType.fromBits( (0 until bitWidth).reverse.map( x => b(x) ).reduce(_##_) )
  }

  // 2. Pipelined DSP multiply
  def dspMultiply( op1 : Fixed, op2 : Fixed, regIn : Int, regOut : Int): Fixed = {
    val a = ShiftRegister( op1.toSInt, regIn )
    val b = ShiftRegister( op2.toSInt, regIn )
    val out = ShiftRegister( a * b, aStages - regIn ).toUInt
    convToFixed( out )
  }

  def cosFunc( op1 : Fixed ): Fixed = {
    val cosFixed = Vec( cosine.map( x => Fixed(x, bitWidth, fracWidth) ) ) // ROM
    val idx = op1( (bitWidth - intWidth) , (fracWidth - lutFracWidth) )
    val a = ShiftRegister( idx, 1)
    val out = ShiftRegister( cosFixed(a), aStages - 1 )
    //out
    ShiftRegister( op1+op1, 3) //************************************************
  }

  val alu_out = Fixed(width=bitWidth, fracWidth=fracWidth)
  alu_out := MuxCase( Fixed(0, bitWidth, fracWidth), Array(
        ( aluCode === UInt(8) ) -> ShiftRegister(op1, aStages), // op1, //
        ( aluCode === UInt(0) || 
          aluCode === UInt(1) ||
          aluCode === UInt(12 )) -> adderStage( op1, op2 ), //(op1 + op2), //
        ( aluCode === UInt(2) || 
          aluCode === UInt(3) ||
          aluCode === UInt(4) ||
          aluCode === UInt(13) ) -> dspMultiply( op1, op2, 1, 2 ),
        ( aluCode === UInt(5) ) -> cosFunc( op1 )  
                    ))

  dat_out := alu_out

  val sumCode = RegNext( aluCode )
  sum_local := Mux( (sumCode === UInt(4)), (sum_local + dat_out), Fixed(0, bitWidth, fracWidth) )

  /*
  when( sumCode === UInt(6) ){
    sum_global := sum_local
  }*/

  io.xout := x_parr
  io.hout := dat_out
  io.sout := sum_local //(sReg + sum_global)
  
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

  // hardcode the register pattern for lvl = {2, 3, 4}
  def parity( x : Int ) : Boolean = {
    var k = 0
    var d = x
    while( d != 0 ){
      k = k + 1
      d = d & (d-1)
    }
    ( k%2 == 0)
  }

  val a = ( 0 until b ).map( x => (0 until log2Up(b) ).reverse.map( y => parity( x & ( pow(2,y).toInt ) ) ) )

  //val a4 = List( List(1,1), List(1,0), List(0,1), List(0,0) )

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

class AddrGen( k : Int ) extends Module{

  val io = new Bundle{
    val counter = UInt(INPUT, width=log2Up(k) )
    val func = UInt(INPUT, width=4)
    val rdAddr = UInt(OUTPUT, width=log2Up(k)+3 )
    val wrAddr = UInt(OUTPUT, width=log2Up(k)+3 )
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

  val rDefault = (io.func(2,0) ## io.counter)
  val rHad = (UInt(0, 2) ## toggle ## hadGen)
  val rSel = ( io.func === UInt(1, 4) ) 

  val rdAddr = UInt(width=log2Up(k)+3)
  rdAddr := Mux( ( io.func === UInt(1, 4) ), rHad, rDefault )


  // break path up, add a register here, and reduce delay by 1
  val wToggle = RegNext(!toggle)
  val wHadGen = RegNext( io.counter )
  val wDefault = RegNext( rdAddr )
  val wSel1 = RegNext( (io.func === UInt(1,4)) )
  val wSel2 = RegNext( (io.func === UInt(2,4)) ) // write to addr 0

  val wHad = ( UInt(0, 2) ## wToggle ## wHadGen )
  val wrAddr = UInt(width=log2Up(k)+3)
  wrAddr := MuxCase( wDefault, Array( 
                ( wSel1 ) -> wHad, 
                ( wSel2 ) -> ( UInt(0, 3) ## wHadGen ) 
                    ))


  // delay = (from func -> dat_out) = mStages + aStages + eStages

  // connect outputs
  io.rdAddr := rdAddr
  io.wrAddr := wrAddr // *** includes a delay of 1
  io.lvl := lvl


}