package linear

import Chisel._
import com.github.tototoshi.csv._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.util.Random

import utils._

import math._

/*
Fastfood Experimental Version:
  - Does not implement FWHT
  - Generates the Hadamard coeffs, and computes in O(nd) time
  - Linear Array, with two overlapping datapaths
  - not working for n==p 
*/

class PE( val id : Int, val bitWidth : Int, val fracWidth : Int,
  val n : Int, val p : Int, val d : Int, g : Seq[BigInt], s : Seq[BigInt], 
    alpha : Seq[BigInt], val aStages : Int, cosFlag : Boolean = false, forSim : Boolean = true, 
        seed : Int = 45687 ) extends Module{
  
  val io = new Bundle{
    val xin = Fixed(INPUT, bitWidth, fracWidth)
    val pin = Fixed(INPUT, bitWidth, fracWidth)
    val hin = Fixed(INPUT, bitWidth, fracWidth)
    val sin = Fixed(INPUT, bitWidth, fracWidth)
    val yin = Fixed(INPUT, bitWidth, fracWidth)
    
    val xout = Fixed(OUTPUT, bitWidth, fracWidth)
    val pout = Fixed(OUTPUT, bitWidth, fracWidth)
    val hout = Fixed(OUTPUT, bitWidth, fracWidth)
    val sout = Fixed(OUTPUT, bitWidth, fracWidth)
    val yout = Fixed(OUTPUT, bitWidth, fracWidth)

    val ctrl = new CtrlIO().flip()

  }

  // 512 x bitWidth ROMs for Cosine LUT
  def cosTable(A : Double, b: Double, n : Int = 512) = {

    val tab = (0 until n).map(x => x/(1<<(log2Up(n)-1) ).toDouble )
              .map(y => A*cos(y*Pi + b) )
    val fixedTab = tab.map( x => toFixed(x, fracWidth) ).toVector
    fixedTab
  }

  // architecture parameters
  val regIn = 1
  val regOut = 2

  // local pipeline register for ctrl signals
  val prst = Reg( init=Bool(false), next=io.ctrl.prst )
  val pload = Reg( init=Bool(false), next=io.ctrl.pload )
  //val pinc = Reg( init=Bool(false), next=io.ctrl.pinc )
  val padd = Reg( init=Bool(false), next=io.ctrl.padd )
  val dload = Reg( init=Bool(false), next=io.ctrl.dload )
  val dadd = Reg( init=Bool(false), next=io.ctrl.dadd )
  val func = Reg( init=UInt(0, 4), next=io.ctrl.func )


  // ----------------------------------------------------------------------------------//

  // xin
  val x_data = RegInit( Fixed(0, bitWidth, fracWidth) )
  val x_parr = RegInit( Fixed(0, bitWidth, fracWidth) )
  x_parr := io.xin
  when( pload ){
    x_data := x_parr
  }

  // ----------------------------------------------------------------------------------//
  // datapath 1
  val dat_out = RegInit( Fixed(0, bitWidth, fracWidth) )
  val pReg = Fixed(width=bitWidth, fracWidth=fracWidth)
  if ( p == n ){
    pReg := io.pin
  }else{
    pReg := ShiftRegister( io.pin, (n/p)-1 )
  }
  

  val phb = Module( new LFSR( 45687 ) )
  phb.io.en := padd
  phb.io.rst := !padd
  val index = phb.io.out
  

  // operand two for different functions
  val op_phb = Fixed( width=bitWidth, fracWidth=fracWidth )
  op_phb := x_data
  when( !index ){
    op_phb := -x_data
  }

  // decode operands
  val op1 = Fixed( width=bitWidth, fracWidth=fracWidth )  
  op1 := pReg

  val op2 = Fixed( width=bitWidth, fracWidth=fracWidth )  
  op2 := op_phb

  // binary multiply accumulate
  val s1_out = Fixed( width=bitWidth, fracWidth=fracWidth )
  s1_out := (op1 + op2)  
  dat_out := s1_out

  // connect output ports
  io.xout := x_parr
  io.pout := dat_out
  when( prst ){
    io.pout := RegInit( Fixed( 0, bitWidth, fracWidth ) )
  }

  //------------------------------------------------------------------------------------//
  // datapath 2:
  val mStages = 2*regIn + regOut //mem stages 
  val oStages = 1 // operand before alu
  // aStages = 3 //3 // alu
  
  val hStages =  max(0, n/p -1 - oStages - aStages) // length of hReg
  val rStall = ( hStages == 0 ) // true or false
  val sCycles = abs( n/p -1 - oStages - aStages ) //stall cycles

  // Coisne table parameters
  val intWidth = (bitWidth - fracWidth)
  val lutWidth = log2Up( 512 ) // 2**(lutWidth) = lutSize entries
  val lutFracWidth = lutWidth - 1 // only one bit for integer (mod 2)

  val paramBits = 3 // 2bits required for pointing to g,s,alpha,k mem blocks


  // Build Memory 
  var addrWidth = paramBits + log2Up(n/p) //only require 3bits of func
  if( n==p ){
    addrWidth = addrWidth-1
  }
  if( cosFlag ){
    addrWidth = lutWidth + max(1, addrWidth-lutWidth) 
  }

  val cosine = cosTable( 1.0, 0, 512 )
  //val cosine = ( 0 until 512 ).map( x=> BigInt(4096) ).toVector
  val kern = (0 until n/p).map( x => BigInt(0) ).toVector
  val extra = (0 until n/p).map( x => BigInt(0) ).toVector

  var mem = g ++ s ++ alpha ++ kern
  if( cosFlag ){
    mem = cosine ++ mem
  }
  if( paramBits > 2){
    mem = mem ++ extra
  }

  val res_out = RegInit( Fixed(0, bitWidth, fracWidth) )
  val dataMem = Module( new PipelinedDualPortBRAM( Fixed(width=bitWidth, fracWidth=fracWidth), 
                                            addrWidth, regIn, regOut, id, mem, forSim ) )


  // Counter for indexing each neuron in the PE
  val addr = RegInit( UInt( 0, width=log2Up(n/p) ) )
  when( dadd ){
    if( n==p ){
      addr := addr
    }else{
      addr := addr + UInt(1)
    }  
  }

  // Read address and Opcode register
  var rAddr = (func(paramBits-1,0) ## addr)
  if( cosFlag ){
    rAddr = ( UInt(1,1) ## UInt(0, lutWidth - paramBits - addr.getWidth) ## func(paramBits-1,0) ## addr )
  }

  //println( rAddr.getWidth, addrWidth )

  val opCode = ShiftRegister( func, mStages )
  val aluCode = ShiftRegister( opCode, oStages + aStages )

  dataMem.io.ports(0).req.addr := rAddr
  dataMem.io.ports(0).req.writeData := x_parr
  dataMem.io.ports(0).req.writeEn := dload


  // -----------------------------------------------------------------
  // Important control logic for: 
  //    i.)   Mem write back 
  //    ii.)  Reset hReg between multiplying g and hadamard, for n/p-1 cycles
  //    iii.) Starting Hadamard

  val hadOn = ( func === UInt(3,4) )
  val upW = ShiftRegister( ( opCode === UInt(0, 4) || opCode === UInt(4, 4) ||
                             opCode === UInt(10, 4) ), 1 + oStages + aStages )
  val dnW = ShiftRegister( upW, n/p )
  val write = ( upW ) //&& !dnW )
  val hadOnDelayed = RegNext( hadOn )
  val upRst = ShiftRegister( ( opCode === UInt(0, 4) ), 1 + oStages + aStages )
  val dnRst = ShiftRegister( upRst, n/p )
  val rst = ( upRst && !dnRst ) 
  val rstG = Bool()
  if( hStages == 0 ){
    rstG := rst
  }else{
    rstG := ShiftRegister( rst, n/p -1 - oStages - aStages ) //Bool()   
  }

  // -----------------------------------------------------------------
  // Connect Operand 1

  // 1. Intermediate Op1: Connect to loopback hadamard or local res_out
  val passActive = ShiftRegister(aluCode, 1 ) // hadamard ends with valid data in the previous/adjacent PE
  val i1op1 = Mux( ( aluCode === UInt(3, 4) || passActive === UInt(3,4) ), io.hin, res_out )
  
  // LutRAM Shift Register - holding (n/p-1) neurons
  val hReg = Fixed(width=bitWidth, fracWidth=fracWidth)
  if( hStages == 0 ){
    hReg := i1op1
  }else{
    hReg := ShiftRegister( i1op1, hStages )
  }

  // sum pass through lane
  val sReg = RegInit( Fixed(0, width=bitWidth, fracWidth=fracWidth) ) 
  sReg := io.sin

  // y register for computing the error
  val yreg = RegInit( Fixed(0, width=bitWidth, fracWidth=fracWidth) )
  val y_parr = RegInit( Fixed(0, width=bitWidth, fracWidth=fracWidth) )
  y_parr := io.yin
  val y_data_1 = RegInit( Fixed(0, width=bitWidth, fracWidth=fracWidth) ) //ShiftRegister( y_parr, 2, en=pload )
  val y_data_2 = RegInit( Fixed(0, width=bitWidth, fracWidth=fracWidth) )
  when( pload ){
    y_data_1 := y_parr
    y_data_2 := y_data_1
  }

  yreg := -y_data_2
  //when( opCode === UInt(6, 4) ){
  //  yreg := -y_data
  //}

  val eta = RegInit( Fixed( toFixed(-0.01, fracWidth), bitWidth, fracWidth ) )
  val delta = RegInit( Fixed(0, bitWidth, fracWidth) )
  // 2. Intermediate Op1: Connect hReg, dat_out, sReg, yreg
  val i2op1 = Fixed(width=bitWidth, fracWidth=fracWidth)
  i2op1 := MuxCase( hReg, Array(
    ( opCode === UInt(0, 4) ) -> dat_out,
    ( opCode === UInt(9, 4) || opCode === UInt(11, 4) ) -> delta
                  ))

  if( cosFlag ){
    val tranSreg = ShiftRegister( res_out, n/p -1 )
    when( opCode === UInt(2) ){
      i2op1 := tranSreg
    }
  }


  // 3. Op1: Connect to intermediate operand, else if rstG is high, reset op1 to initialise hadamard
  val op11 = RegInit( Fixed(0, bitWidth, fracWidth) ) // 
  op11 := Mux( rstG, Fixed(0, bitWidth, fracWidth), i2op1 ) //io.rstG

  // if Cosine LUT is combined with dataMem
  if( cosFlag ){
    when( opCode === UInt(4) ){
      dataMem.io.ports(0).req.addr := i2op1((bitWidth - intWidth) , (fracWidth - lutFracWidth))
    }
  }
  
  // ---------------------------------------------------------------------------------------------------
  // Operand 2 (much more complicated)

  // Read data from Memory (Contains scaling matrices G, S, alpha weights, and the kernel feature map)
  val gsak = dataMem.io.ports(0).rsp.readData
  
  // Sum all neurons in one PE, and store in register
  val sum_global = RegInit( Fixed(0, bitWidth, fracWidth ) )
  val sum_local = RegInit( Fixed(0, bitWidth, fracWidth ) )

  // ----------------------------------------------------------------
  // Hadamard Coefficient Generator:
  
  val ld = d*p/n // number of PEs per hadamard transform
  val n_fix = UInt( width=(log2Up(d) ) )
  //val n_new = RegInit( UInt( width=log2Up( d ) ) )
  if( n==p ){
    n_fix := UInt( id%ld, width=log2Up(d) ) //only one neuron per PE
  }else if( ld>1 ){
    n_fix := ( UInt( id%ld, width=(log2Up(d)-addr.getWidth) ) ## addr )
  }else{
    n_fix := addr
  }
  var new_shift = addr.getWidth
  if( n==p ){
    new_shift = 0
  }
  val n_new = RegInit( UInt( id%ld << new_shift, width=log2Up( d ) ) ) 
  
  // n_fix -> loops over the columns (neuron address, slower)
  // n_new -> loops over the rows
  //    eg. read neuron address from n_fix, loop through n_new, increment n_fix and move to next neuron
  when( hadOn ){
    n_new := n_new + UInt(1)
  }

  val hcg = RegInit( Bool(false) )
  val tmp = n_new & n_fix  // bitwise AND gate
  hcg := ShiftRegister( xorR(tmp), mStages-1 ) //minus one because hcg is latched
  val op_had = Fixed(width=bitWidth, fracWidth=fracWidth)
  op_had := Mux( hcg, -gsak, gsak )

  // --------------------------------------------------------------

  // Op2: gsak, op_had, sum,
  val op22 = RegInit( Fixed(0, bitWidth, fracWidth) )  
  op22 := MuxCase( gsak, Array( 
    ( opCode === UInt(3, 4) ) -> op_had,
    ( opCode === UInt(9, 4) ) -> eta, 
    ( opCode === UInt(8, 4) ) -> Fixed(0, bitWidth, fracWidth)
                  ))

  // ----------------------------------------------------------------------------------------------------
  // ALU
  // 1. BRAM LUT for Cosine function
  if( cosFlag ){
    Predef.assert( aStages>=(mStages-1), "Error: Can NOT initialise cosine BRAM for aStages<(mStages-1)" )
  }
  def cosFunc( op1 : Fixed ): Fixed = {

    if( !cosFlag ){
      val cosFixed = Vec( cosine.map( x => Fixed(x, bitWidth, fracWidth) ) ) // ROM
      val idx = op1( (bitWidth - intWidth) , (fracWidth - lutFracWidth) )
      val a = ShiftRegister( idx, regIn)
      val out = ShiftRegister( cosFixed(a), aStages - regIn )
      out
    } else{
      if( aStages == mStages-1 ){
        gsak
      } else{
        ShiftRegister( gsak, max( 0, aStages - (mStages - 1) ) )
      }
    }
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


  def adderStage( op1 : Fixed, op2 : Fixed ): Fixed = {
    val a = RegNext( op1 )
    val b = RegNext( op2 )
    val out = ShiftRegister( (a + b), aStages - 1 )
    out
  }

  // 3. Connect everything
  val alu_out = Fixed(width=bitWidth, fracWidth=fracWidth)
  alu_out := MuxCase( Fixed(0, bitWidth, fracWidth), Array(
    ( aluCode === UInt(0, 4) || aluCode === UInt(1, 4) || 
      aluCode === UInt(2, 4) || aluCode === UInt(9, 4) || 
      aluCode === UInt(11, 4) ) -> dspMultiply(op11, op22, 1, 2), 
    ( aluCode === UInt(3, 4) || aluCode === UInt(10, 4) ) -> adderStage( op11, op22 ),
    ( aluCode === UInt(4, 4) ) -> ( cosFunc( op11 ) )
                      )) 

  // write alu results to a register
  res_out := alu_out

  // ----------------------------------------------------------------------------------------------------
  // Write-back
  val wback = UInt(width=paramBits) //only write back to kernel memory location, i.e. UInt(3,3)
  wback := UInt(3) //kernel

  val rAddrShift = ShiftRegister( addr, mStages + 1 + oStages + aStages) // plus one for res_out 
  var wAddr = ( wback ## rAddrShift )  
  if( cosFlag ){
    wAddr = ( UInt(1,1) ## UInt(0, lutWidth - paramBits - addr.getWidth) ## wback ## rAddrShift )
  }

  dataMem.io.ports(1).req.addr := wAddr 
  dataMem.io.ports(1).req.writeData := res_out
  dataMem.io.ports(1).req.writeEn := write


  // Connecting output
  io.hout := res_out
  io.sout := sReg + sum_global 
  io.yout := y_parr

  // compute the sum and error/delta
  val prCode = passActive
  sum_local := Mux( ( prCode === UInt(2,4) ), res_out + sum_local, Fixed(0, bitWidth, fracWidth) ) 

  // one cycle - save intermediate sum
  when( ( prCode === UInt(5, 4) ) ){
    sum_global := sum_local
  }
  
  // compute error, and reset
  when( (prCode === UInt(8, 4)) ){
    delta := (yreg + sReg)
    sum_global := Fixed(0, bitWidth, fracWidth) //reset
    sReg := Fixed(0, bitWidth, fracWidth) //reset
  }

  when( prCode === UInt(9, 4) ){
    delta := res_out
  }

  // write-back to alpha mem block
  when( prCode === UInt(10) ){
    wback := UInt(2)
  }

}
