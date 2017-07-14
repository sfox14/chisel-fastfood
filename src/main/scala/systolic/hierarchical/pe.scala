package hierarchical

import Chisel._
import com.github.tototoshi.csv._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.util.Random

import utils._
import xilinx._

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

class BramSreg[T <: Fixed](gen : T, n : Int, a : Int, forSim : Boolean) extends Module{
	/*
	n = length of sreg
	a = mStages + aStages + eStages
	*/

	val fixedType = gen.cloneType
	val io = new Bundle{
		val in = Fixed( INPUT, fixedType.getWidth, fixedType.fractionalWidth )
		val out = Fixed( OUTPUT, fixedType.getWidth, fixedType.fractionalWidth ) 
	}

	val mem = (0 until pow(2, log2Up(n+1)).toInt ).map( x => BigInt(0) ).toVector
	val sreg = Module( new PipelinedDualPortBRAM( fixedType,
                                log2Up(n+1), 1, 2, 2000, mem, forSim ) )

	val rAddr = RegInit( UInt(0, log2Up(n+1)) )
	rAddr := rAddr + UInt(1)
	when( rAddr === UInt(n, log2Up(n+1)) ){
		rAddr := UInt(0)
	}
	val wAddr = ShiftRegister(rAddr, a)

	// port0 - read
	sreg.io.ports(0).req.addr := rAddr
	io.out := sreg.io.ports(0).rsp.readData

	// port 1 - write
	sreg.io.ports(1).req.addr := wAddr 
	sreg.io.ports(1).req.writeData := io.in
	sreg.io.ports(1).req.writeEn := Bool(true)


}





// PE version with Fast Hadamard Transform

class PE( val id : Int, val hid : Int, val bitWidth : Int, val fracWidth : Int,
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

  // local pipeline register for ctrl signals
  val pload = Reg( init=Bool(false), next=io.ctrl.pload )
  val padd = Reg( init=Bool(false), next=io.ctrl.padd )
  val func = Reg( init=UInt(0, 4), next=io.ctrl.func )
  val sx = Reg( init=UInt(0, 3), next=io.ctrl.sx )


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
  val sum_local = RegInit( Fixed(0, bitWidth, fracWidth ) )


  // PE memory
  //val porig = ShiftRegister( dat_out, k - aStages - eStages - 1 - 3)
  val porig = Module( new BramSreg( Fixed(width=bitWidth, fracWidth=fracWidth), 
  							k - aStages - eStages - 3,
  							aStages + eStages, forSim) )
  porig.io.in := dat_out
  val preg = ShiftRegister( porig.io.out, 3 )
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

  val dataMem = Module( new PipelinedDualPortBRAM( Fixed(width=bitWidth, fracWidth=fracWidth),
                                        aw, 1, 2, id, mem, forSim ) ) 

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
  agen.io.porig := porig.io.out
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
          aluCode === UInt(5) ) -> dspMultiply( op1, op2, 1, 2 )  
                    ))

  dat_out := alu_out

  val sumCode = RegNext( aluCode )
  val dat_out_reg = RegNext( dat_out ) //reduce critical path
  val sel_sl = Bool()
  sel_sl := RegNext( (sumCode === UInt(4)) )
  sum_local := Mux( sel_sl, (sum_local + dat_out_reg), Fixed(0, bitWidth, fracWidth) )

  io.xout := x_parr
  io.hout := dat_out
  io.sout := sum_local
  
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

  val a = ( 0 until b ).map( x => (0 until log2Up(b) ).reverse.map( y => parity( x & ( pow(2,y).toInt ) ) ) )

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

  /*                                                   Addresses with Data                 Init BRAM
  * k=256, 11-bits, cosine in    110 XXXXXXXX,   (000, 001, 010, 011, 100, 101)       H1 H2 G S a 0     (256)  cos (256) in 110
  * k=128, 10-bits, cosine in    11X XXXXXXX,     all except (110) and (111)          H1 H2 G S a 0     (128)  cos (256) in 11
  * k=64,  10-bits, cosine in  1 0XX XXXXXX,              all                         H1 H2 G S a 0 E   (64)   cos (256) in 10 
  * k=32,  10-bits, cosine in 01 XXX XXXXX,               all                         H1 H2 G S a 0 E E (32)   cos (256) in 01
  */
     // Cosine table parameters
  val intWidth = (bitWidth - fracWidth)
  val lutWidth = log2Up( 256 ) // 2**(lutWidth) = lutSize entries
  val lutFracWidth = lutWidth - 1 // only one bit for integer (mod 2)
  val cosAddr = io.porig( (bitWidth - intWidth) , (fracWidth - lutFracWidth) )

  // Default for k=32
  var rCos = UInt(1,2) ## cosAddr
  var rDefault = UInt(0,2) ## io.func(2,0) ## io.counter
  var rHad = UInt(0,2) ## UInt(0,1) ## toggle ## hadGen

  if( k==64 ){
    rCos = UInt(2,2) ## cosAddr
    rDefault = UInt(0,1) ## io.func(2,0) ## io.counter
    rHad = UInt(0,1) ## UInt(0,1) ## toggle ## hadGen
  }

  if( k==128 ){
    rCos = UInt(3,2) ## cosAddr
    rDefault = (io.func(2,0) ## io.counter)
    rHad = UInt(0,1) ## toggle ## hadGen
  }

  if( k==256 ){
    rCos = UInt(6,3) ## cosAddr
    rDefault = (io.func(2,0) ## io.counter)
    rHad = UInt(0,1) ## toggle ## hadGen
  }


  /*
  val rCos = (io.func(2,0) ## io.counter)
  val rDefault = (io.func(2,0) ## io.counter)
  val rHad = (UInt(0, 2) ## toggle ## hadGen)
  val rSel = ( io.func === UInt(1, 4) ) 
  */

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

  // delay = (from func -> dat_out) = mStages + aStages + eStages

  // connect outputs
  io.rdAddr := rdAddr
  io.wrAddr := wrAddr // *** includes a delay of 1
  io.lvl := lvl

}