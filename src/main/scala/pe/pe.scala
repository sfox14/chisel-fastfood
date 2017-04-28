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



  // local pipeline register for ctrl signals
  val prst = Reg( init=Bool(false), next=io.ctrl.prst )
  val pload = Reg( init=Bool(false), next=io.ctrl.pload )
  val pinc = Reg( init=Bool(false), next=io.ctrl.pinc )
  val padd = Reg( init=Bool(false), next=io.ctrl.padd )
  val dload = Reg( init=Bool(false), next=io.ctrl.dload )
  val dadd = Reg( init=Bool(false), next=io.ctrl.dadd )
  val func = Reg( init=UInt(0, 3), next=io.ctrl.func )




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
  

  // local ctrl signals
  val countVals = (0 until n).grouped( n/p ).toList
  var initVal = ( countVals.last :: countVals.dropRight(1) )(id)(0)
  if ( n==p ){
    initVal = (countVals.drop(1) ::: List(countVals.head))(id)(0)
  }

  val cp = RegInit( UInt( initVal, width=log2Up( n ) ) ) //*could be problem here*
  val cd = RegInit( UInt( 0, width=log2Up( (d/p) ) ) )
  when( padd ){ //padd //!stall
    cp := cp + UInt(1)
  }
  when( pinc ){
    cd := cd + UInt(1)
  }
  val wpAddr = UInt( width=log2Up(n*d/p) ) 
  if( d==p ){
    wpAddr := cp
  }else{
    wpAddr := (cd ## cp)
  }  

  // memory and register files
  val phb = Vec( ram.map( (i: Int) => Bool(i==1) ) )
  //val index = phb( wpAddr ) //  RegNext( RegNext( phb( wpAddr ) ) ) //
  val index = RegNext( RegNext( phb( wpAddr ) ) )
  

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

  // basic alu
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
  val addrWidth = func.getWidth + log2Up(n/p)
  val res_out = RegInit( UInt(0, bitWidth) )

  val dataMem = Module( new PipelinedDualPortBRAM( addrWidth, bitWidth, 1, 2 ) )

  // local ctrl signals
  val addr = RegInit( UInt( 0, width=log2Up(n/p) ) )
  when( dadd ){
    if( n==p ){
      addr := addr
    }else{
      addr := addr + UInt(1)
    }  
  }
  val rAddr = (func ## addr)
  val opCode = ShiftRegister( func, 3+1 ) //RegInit( UInt(3, width=3) )


  dataMem.io.ports(0).req.addr := rAddr
  dataMem.io.ports(0).req.writeData := x_parr
  dataMem.io.ports(0).req.writeEn := dload

  // sum pass through lane
  val sReg = RegInit( UInt(0, width=bitWidth) )
  sReg := io.sin

  // operand 1
  val passActive = ShiftRegister(func, 4 + 1 )
  val hrg = MuxCase( res_out, Array(
                ( opCode === UInt(3, 3) || passActive === UInt(3,3) ) -> io.hin 
                ))
  val hReg = ShiftRegister( hrg, (n/p)-1 )



  // write, hon, rstG decode
  val hon = ( func === UInt(3,3) )
  val upW = RegNext( ( opCode === UInt(0, 3) ) )
  val dnW = ShiftRegister( upW, n/p )
  val write = ( upW && !dnW )
  val hon1 = RegNext( hon )
  val rstG = Bool()
  if( n==p ){
    rstG := ( hon && !hon1 )
  }else{
    rstG := ShiftRegister( write, n/p -1 )
  }


  val op11_tmp = UInt(width=bitWidth)
  op11_tmp := MuxCase( hReg, Array(
                  ( opCode === UInt(0, 3) ) -> dat_out, 
                  ( opCode === UInt(6, 3) ) -> sReg
                  ))

  val op11 = UInt(width=bitWidth) // RegInit( UInt(0, width=bitWidth) ) //********
  op11 := Mux( rstG, UInt(0, bitWidth), op11_tmp ) //io.rstG

  // operand 2 (much more complicated)
  val gsak = dataMem.io.ports(0).rsp.readData
  

  // hadamard coefficient generator:
    //  ld - number of hadamards per PE
  val ld = d*p/n
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
  println(s"PE_$id: ", id%ld)
  
  // n_fix -> loops over the columns (neuron address, slower)
  // n_new -> loops over the rows
  //    eg. read neuron address from n_fix, loop through n_new, increment n_fix and move to next neuron
  when( hon ){ //io.hon
    n_new := n_new + UInt(1)
  }

  val hcg = RegInit( Bool(false) )
  val tmp = n_new & n_fix  // bitwise AND gate
  // use a shift-register here (equal to BRAM pipeline registers)
  hcg := ShiftRegister( xorR(tmp), 3 )
  val op_had = UInt(width=bitWidth)
  op_had := Mux( hcg, -gsak, gsak )


  // sum register
  val sum = RegInit( UInt(0, width=bitWidth) )

  val yreg = RegInit( UInt(0, width=bitWidth) )
  val y_parr = RegInit( UInt(0, width=bitWidth) )
  y_parr := io.yin
  val y_data = ShiftRegister( y_parr, 2, en=pload )
  when( opCode === UInt(6,3) ){
    yreg := y_data
  }


  val op22 = UInt(width=bitWidth) // RegInit( UInt(0, width=bitWidth) ) //**********
  op22 := MuxCase( gsak, Array(
                  ( opCode === UInt(3, 3) ) -> op_had,
                  ( opCode === UInt(5, 3) || opCode === UInt(6,3) ) -> sum 
                  ))

  // test
  def cosFunc( op1 : UInt ): UInt = {
    op1 + op1
  }

  //****
  def dspMultiply( op1 : UInt, op2 : UInt, regIn : Int, regOut : Int): UInt = {
    val a = ShiftRegister( op1.toSInt, regIn )
    val b = ShiftRegister( op2.toSInt, regIn )
    val out = ShiftRegister( a * b, regOut ).toUInt
    op1*op2 //out ******************************************
  }


  val alu_out = UInt(width=bitWidth)
  alu_out := MuxCase( UInt(0, bitWidth), Array(
    ( opCode === UInt(0, 3) || ( opCode === UInt(1, 3) ) || ( opCode === UInt(2, 3) ) ) -> dspMultiply(op11, op22, 1, 2), //RegNext(RegNext(RegNext( RegNext(RegNext(op11)) * RegNext(RegNext(op22)) ))), 
    ( opCode === UInt(3, 3) || ( opCode === UInt(5, 3) ) || ( opCode === UInt(6, 3) ) ) -> ( op11 + op22 ),
    ( opCode === UInt(4, 3) ) -> ( cosFunc( op11 ) )
                      )) 


  res_out := alu_out

  val wAddr = ShiftRegister( addr, 2+3 ) // variable length ShiftRegisters

  dataMem.io.ports(1).req.addr := ( UInt(3,3) ## wAddr )
  dataMem.io.ports(1).req.writeData := res_out
  dataMem.io.ports(1).req.writeEn := write //io.write

  when( opCode === UInt(5, 3) ){
    sum := alu_out //res_out //alu_out ***************************************
  }
  
  io.hout := res_out
  io.sout := UInt(0, bitWidth)
  io.yout := y_parr
  
  when( opCode === UInt(6,3) ){
    io.sout := alu_out //res_out (*try alu_out, but may not hit 500MHz, lose 1 cycle/pe using res_out)
  }
  
  // can either go io.sout := alu_out or op11 := io.sin to lose the extra register

}

