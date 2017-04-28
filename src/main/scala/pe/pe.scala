package pe

import Chisel._
import com.github.tototoshi.csv._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.util.Random

import fpgatidbits.ocm._

import utils._

import math._



class PE( val id : Int, val bitWidth : Int, val fracWidth : Int,
              val ram : List[Int], val n : Int, val p : Int, val d : Int ) extends Module{
  val io = new Bundle{
    val xin = UInt(INPUT, bitWidth)
    val pin = UInt(INPUT, bitWidth)
    val hin = UInt(INPUT, bitWidth)
    val sin = UInt(INPUT, bitWidth)
    val yin = UInt(INPUT, bitWidth)
    
    val xout = UInt(OUTPUT, bitWidth)
    val pout = UInt(OUTPUT, bitWidth)
    val hout = UInt(OUTPUT, bitWidth)
    val sout = UInt(OUTPUT, bitWidth)
    val yout = UInt(OUTPUT, bitWidth)

    val ctrl = new CtrlIO().flip()

  }

  println(s"PE_$id: ", ram)

  // architecture parameters
  val regIn = 1
  val regOut = 2
  val oCycles = 2*regIn + regOut


  // local pipeline register for ctrl signals
  val prst = Reg( init=Bool(false), next=io.ctrl.prst )
  val pload = Reg( init=Bool(false), next=io.ctrl.pload )
  val pinc = Reg( init=Bool(false), next=io.ctrl.pinc )
  val padd = Reg( init=Bool(false), next=io.ctrl.padd )
  val dload = Reg( init=Bool(false), next=io.ctrl.dload )
  val dadd = Reg( init=Bool(false), next=io.ctrl.dadd )
  val func = Reg( init=UInt(0, 3), next=io.ctrl.func )


  // ----------------------------------------------------------------------------------//

  // xin
  val x_data = RegInit( UInt(0, bitWidth) )
  val x_parr = RegInit( UInt(0, bitWidth) )
  x_parr := io.xin
  when( pload ){
    x_data := x_parr
  }

  // ----------------------------------------------------------------------------------//
  // datapath 1
  val dat_out = RegInit( UInt(0, bitWidth) )
  val pReg = UInt(width=bitWidth)
  if ( p == n ){
    pReg := io.pin
  }else{
    pReg := ShiftRegister( io.pin, (n/p)-1 )
  }
  

  // Initial value of the PE counter, according to its id 
  val countVals = (0 until n).grouped( n/p ).toList
  var initVal = ( countVals.last :: countVals.dropRight(1) )(id)(0)
  if ( n==p ){
    initVal = (countVals.drop(1) ::: List(countVals.head))(id)(0)
  }

  // Counters for computing phbx dot product
  //    cp - points to a column of the phb binary matrix
  //    cd - if d>p, increment to compute on the next block of the d*n matrix
  val cp = RegInit( UInt( initVal, width=log2Up( n ) ) )
  val cd = RegInit( UInt( 0, width=log2Up( (d/p) ) ) )
  when( padd ){
    cp := cp + UInt(1)
  }
  when( pinc ){
    cd := cd + UInt(1)
  }
  // read address from phb ROM
  val wpAddr = UInt( width=log2Up(n*d/p) ) 
  if( d==p ){
    wpAddr := cp
  }else{
    wpAddr := (cd ## cp)
  }  

  // memory and register files
  val phb = Vec( ram.map( (i: Int) => Bool(i==1) ) )
  val index = ShiftRegister( phb( wpAddr ), regOut ) 
  

  // operand two for different functions
  val op_phb = UInt( width=bitWidth )
  op_phb := x_data
  when( !index ){
    op_phb := -x_data
  }

  // decode operands
  val op1 = UInt( width=bitWidth ) //RegInit( UInt(0, width=bitWidth ) ) // 
  op1 := pReg

  val op2 = UInt( width=bitWidth ) // RegInit( UInt(0, width=bitWidth ) ) // 
  op2 := op_phb

  // binary multiply accumulate
  val s1_out = UInt( width=bitWidth )
  s1_out := (op1 + op2) //Mux( io.s1, (op1 + op2), dat_out ) 
  dat_out := s1_out

  // connect output ports
  io.xout := x_parr
  io.pout := dat_out
  when( prst ){
    io.pout := RegInit( UInt( 0, bitWidth ) )
  }

  //------------------------------------------------------------------------------------//
  // datapath 2:
  

  // Result register and Memory
  val addrWidth = func.getWidth + log2Up(n/p)
  val res_out = RegInit( UInt(0, bitWidth) )
  val dataMem = Module( new PipelinedDualPortBRAM( addrWidth, bitWidth, regIn, regOut ) )

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
  val rAddr = (func ## addr)
  val opCode = ShiftRegister( func, oCycles ) //RegInit( UInt(3, width=3) )

  dataMem.io.ports(0).req.addr := rAddr
  dataMem.io.ports(0).req.writeData := x_parr
  dataMem.io.ports(0).req.writeEn := dload

  // -----------------------------------------------------------------
  // Important control logic for: 
  //    i.)   Mem write back 
  //    ii.)  Reset hReg between multiplying g and hadamard, for n/p-1 cycles
  //    iii.) Starting Hadamard

  val hadOn = ( func === UInt(3,3) )
  val upW = RegNext( ( opCode === UInt(0, 3) ) )
  val dnW = ShiftRegister( upW, n/p )
  val write = ( upW && !dnW )
  val hadOnDelayed = RegNext( hadOn )
  val rstG = Bool()
  if( n==p ){
    rstG := ( hadOn && !hadOnDelayed )
  }else{
    rstG := ShiftRegister( write, n/p -1 )
  }
  // -----------------------------------------------------------------
  // Connect Operand 1

  // 1. Intermediate Op1: Connect to loopback hadamard or local res_out
  val passActive = ShiftRegister(opCode, 1 ) // hadamard ends with valid data in the previous/adjacent PE
  val i1op1 = Mux( ( opCode === UInt(3, 3) || passActive === UInt(3,3) ), io.hin, res_out )
  
  // LutRAM Shift Register - holding (n/p-1) neurons
  val hReg = ShiftRegister( i1op1, (n/p)-1 )

  // sum pass through lane
  val sReg = RegInit( UInt(0, width=bitWidth) )
  sReg := io.sin

  // y register for computing the error
  val yreg = RegInit( UInt(0, width=bitWidth) )
  val y_parr = RegInit( UInt(0, width=bitWidth) )
  y_parr := io.yin
  val y_data = ShiftRegister( y_parr, 2, en=pload )
  when( opCode === UInt(6,3) ){
    yreg := y_data
  }

  // 2. Intermediate Op1: Connect hReg, dat_out, sReg, yreg
  val i2op1 = UInt(width=bitWidth)
  i2op1 := MuxCase( hReg, Array(
                  ( opCode === UInt(0, 3) ) -> dat_out, 
                  ( opCode === UInt(6, 3) ) -> sReg
                  ))

  // 3. Op1: Connect to intermediate operand, else if rstG is high, reset op1 to initialise hadamard
  val op11 = UInt(width=bitWidth) // RegInit( UInt(0, width=bitWidth) ) //********
  op11 := Mux( rstG, UInt(0, bitWidth), i2op1 ) //io.rstG

  // ---------------------------------------------------------------------------------------------------
  // Operand 2 (much more complicated)

  // Read data from Memory (Contains scaling matrices G, S, alpha weights, and the kernel feature map)
  val gsak = dataMem.io.ports(0).rsp.readData
  
  // Sum all neurons in one PE, and store in register
  val sum = RegInit( UInt(0, width=bitWidth) )

  // ----------------------------------------------------------------
  // Hadamard Coefficient Generator:
  
  val ld = d*p/n // number of PEs per hadamard transform
  val n_fix = UInt( width=(log2Up(d) ) )
  val n_new = RegInit( UInt( width=log2Up( d ) ) )
  if( n==p ){
    n_fix := UInt( id%ld, width=log2Up(d) ) //only one neuron per PE
  }else if( ld>1 ){
    n_fix := ( UInt( id%ld, width=(log2Up(d)-addr.getWidth) ) ## addr )
  }else{
    n_fix := addr
  }
  if( n==p ){
    n_new := UInt(id%ld, width=log2Up( d ) )
  }else{
    n_new := UInt(id%ld << addr.getWidth, width=log2Up( d ) )
  }
  
  // n_fix -> loops over the columns (neuron address, slower)
  // n_new -> loops over the rows
  //    eg. read neuron address from n_fix, loop through n_new, increment n_fix and move to next neuron
  when( hadOn ){
    n_new := n_new + UInt(1)
  }

  val hcg = RegInit( Bool(false) )
  val tmp = n_new & n_fix  // bitwise AND gate
  hcg := ShiftRegister( xorR(tmp), oCycles-1 ) //minus one because hcg is latched
   val op_had = UInt(width=bitWidth)
  op_had := Mux( hcg, -gsak, gsak )

  // --------------------------------------------------------------

  // Op2: gsak, op_had, sum,
  val op22 = UInt(width=bitWidth) // RegInit( UInt(0, width=bitWidth) ) //**********
  op22 := MuxCase( gsak, Array(
                  ( opCode === UInt(3, 3) ) -> op_had,
                  ( opCode === UInt(5, 3) || opCode === UInt(6,3) ) -> sum 
                  ))

  // ----------------------------------------------------------------------------------------------------
  // ALU

  // 1. BRAM LUT for Cosine function
  def cosFunc( op1 : UInt ): UInt = {
    op1 + op1
  }

  // 2. Pipelined DSP multiply
  def dspMultiply( op1 : UInt, op2 : UInt, regIn : Int, regOut : Int): UInt = {
    val a = ShiftRegister( op1.toSInt, regIn )
    val b = ShiftRegister( op2.toSInt, regIn )
    val out = ShiftRegister( a * b, regOut ).toUInt
    op1*op2 //out 
  }

  // 3. Connect everything
  val alu_out = UInt(width=bitWidth)
  alu_out := MuxCase( UInt(0, bitWidth), Array(
    ( opCode === UInt(0, 3) || ( opCode === UInt(1, 3) ) || ( opCode === UInt(2, 3) ) ) -> dspMultiply(op11, op22, 1, 2), //RegNext(RegNext(RegNext( RegNext(RegNext(op11)) * RegNext(RegNext(op22)) ))), 
    ( opCode === UInt(3, 3) || ( opCode === UInt(5, 3) ) || ( opCode === UInt(6, 3) ) ) -> ( op11 + op22 ),
    ( opCode === UInt(4, 3) ) -> ( cosFunc( op11 ) )
                      )) 

  // write alu result to a register
  res_out := alu_out

  // ----------------------------------------------------------------------------------------------------
  // Write-back

  val wAddr = ShiftRegister( addr, oCycles + 1 ) // plus one for single-cycle ALU
  dataMem.io.ports(1).req.addr := ( UInt(3,3) ## wAddr ) //only write back to kernel memory location, i.e. UInt(3,3)
  dataMem.io.ports(1).req.writeData := res_out
  dataMem.io.ports(1).req.writeEn := write

  // write sum to register
  when( opCode === UInt(5, 3) ){
    sum := alu_out //res_out //alu_out ***************************************
  }
  
  // Connecting output lanes
  io.hout := res_out
  io.sout := UInt(0, bitWidth)
  io.yout := y_parr
  
  when( opCode === UInt(6,3) ){
    io.sout := alu_out //res_out (*try alu_out, but may not hit 500MHz, lose 1 cycle/pe using res_out)
  }
  
  // can either go io.sout := alu_out or op11 := io.sin to lose the extra register

}

