package pe

import Chisel._
import com.github.tototoshi.csv._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.util.Random

import fpgatidbits.ocm._

import utils._

import math._

/*
Similar to Array4.scala but for 8 neurons (dictionaries) and 4 Processing elements:
  - processor with the correct/flexible memory read/write architecture
  - still "Single Function Multiple Data"
*/




class Array42( val bitWidth : Int, val fracWidth : Int, 
    val n : Int, val p : Int, val d : Int ) extends Module {
  val io = new Bundle{
    val xin = UInt(INPUT, bitWidth) //Fixed(INPUT, bitWidth, fracWidth)
    val yin = UInt(INPUT, bitWidth)
    //val func = UInt(INPUT, 6)
    
    val s1 = Bool(INPUT)
    val load = Bool(INPUT)
    val write = Bool(INPUT)
    val prst = Bool(INPUT)
    val incr = Bool(INPUT)
    val proc = Bool(INPUT)

    val init = Bool(INPUT)
    val ih = Bool(INPUT)
    val func = UInt(INPUT, 3)
    val hon = Bool(INPUT)
    val hfon = Bool(INPUT)
    val hrst = Bool(INPUT)
    val rstG = Bool(INPUT)

    val yout = UInt(OUTPUT, bitWidth)
  }

  Predef.assert( p>1, "Error: P must be greater than 1")
  Predef.assert( (d & (d - 1)) == 0, "Error: X dimensionality is NOT a power of 2")
  Predef.assert( p<=d, "Error: Number of PE's should not be greater than dimensionality")
  Predef.assert( (p & (p-1)) == 0, "Error: P is NOT a power of 2" )
  Predef.assert( (n & (n-1)) == 0, "Error: N is NOT a power of 2" ) // required for this implemtation
  Predef.assert( n/p <= d, "Error: A PE can NOT compute partial WHT" )

  val rng = new Random( 47 )

  /*
  val ram = List( List(1,0,1,0),
                  List(1,1,1,1), //) //,
                  List(0,0,1,0),
                  List(1,1,1,1),
                  List(1,0,1,0),
                  List(1,1,1,1), //) //,
                  List(0,0,1,0),
                  List(1,0,1,1)
                   )
  */
  /*
  val ram = List( List(1,0,1,0,1,0,1,0),
                  List(1,1,1,1,1,1,1,1), //) //,
                  List(0,0,1,0,0,0,1,0),
                  List(1,1,1,1,1,1,1,1),
                  List(1,0,1,0,1,0,1,0),
                  List(1,1,1,1,1,1,1,1), //) //,
                  List(0,0,1,0,0,0,1,0),
                  List(1,0,1,1,1,0,1,1)
                   )
  */
  ///*
  val ram = List( List(1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0),
                  List(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1), //) //,
                  List(0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0),
                  List(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1),
                  List(1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0),
                  List(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1), //) //,
                  List(0,0,1,0,0,0,1,0,0,0,1,0,0,0,1,0),
                  List(1,0,1,1,1,0,1,1,1,0,1,1,1,0,1,1)
                   )
  //*/

  Predef.assert( ram.length == d, "Error: D and RAM length does NOT match")
  Predef.assert( ram(0).length == n, "Error: N and RAM(0) length does NOT match")

  val j = (d/p) // number of loads required for one x-vector
  Predef.assert( (j & (j - 1)) == 0, "Error: the number of X loads must be a power of 2")

  val weights = ram.grouped( p ).toList
                  .reduce(_.zip(_).map(x => List(x._1, x._2).flatten ))
                  .reduce( _++_ ).grouped( j*n ).toList // groups j*n neurons per node



  val array = (0 until p).reverse.map( x => Module( new PE42(x, bitWidth, 
                                              fracWidth, weights(x), n, p, d ) ) )


  array(0).io.pin := array(p-1).io.pout 
  array(0).io.sin := array(p-1).io.sout   
  array(0).io.xin := io.xin
  array(0).io.yin := io.yin
  for (ix <- 1 until p){
    array(ix).io.pin := array(ix-1).io.pout
    array(ix).io.sin := array(ix-1).io.sout
    array(ix).io.xin := array(ix-1).io.xout
    array(ix).io.yin := array(ix-1).io.yout
  }
  // connect the hadamard loopback:
  val hadArrays = array.grouped( d*p/n ).toList
  val ld = d*p/n  // number of PEs per hadamard
  for( ix <- 0 until hadArrays.length ){
    hadArrays(ix)(0).io.hin := hadArrays(ix)(ld-1).io.hout
    for( iy <- 1 until ld){
      hadArrays(ix)(iy).io.hin := hadArrays(ix)(iy-1).io.hout
    }
  }
  //array(0).io.hin := array(0).io.hout
  //array(1).io.hin := array(1).io.hout

  // control signals
  for( ix <- 0 until p){
    array( ix ).io.s1 := io.s1
    //array( ix ).io.op_sel := io.op_sel
    array( ix ).io.load := io.load
    array( ix ).io.write := io.write
    array( ix ).io.prst := io.prst
    array( ix ).io.incr := io.incr
    array( ix ).io.proc := io.proc
    array( ix ).io.init := io.init
    array( ix ).io.ih := io.ih
    array( ix ).io.func := io.func
    array( ix ).io.hon := io.hon
    array( ix ).io.hfon := io.hfon
    array( ix ).io.hrst := io.hrst
    array( ix ).io.rstG := io.rstG

  }

  io.yout := array(0).io.hout + array(1).io.hout

}

class PE42( val id : Int, val bitWidth : Int, val fracWidth : Int,
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

    // Control signals
    val s1 = Bool(INPUT)
    val load = Bool(INPUT)
    val incr = Bool(INPUT)
    val proc = Bool(INPUT)

    val init = Bool(INPUT)
    val ih = Bool(INPUT)
    val func = UInt(INPUT, width=3)

  }

  println(s"PE_$id: ", ram)

  // decode ctrl signals
  //val ctrl_op = io.func(2,0)
  //val stall = ( ctrl_func =/= UInt(1, 3) && ctrl_func =/= UInt(3, 3) )

  // xin
  val x_data = RegInit( UInt(0, bitWidth) )
  val x_parr = RegInit( UInt(0, bitWidth) )
  x_parr := io.xin
  when( io.load ){
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
  
  //pReg := io.pin

  // local ctrl signals
  val countVals = (0 until n).grouped( n/p ).toList
  var initVal = ( countVals.last :: countVals.dropRight(1) )(id)(0)
  if ( n==p ){
    initVal = (countVals.drop(1) ::: List(countVals.head))(id)(0)
  }

  val cp = RegInit( UInt( initVal, width=log2Up(n) ) ) //*could be problem here*
  val cd = RegInit( UInt( 0, width=log2Up( (d/p) ) ) )
  when( io.proc ){ //io.proc //!stall
    cp := cp + UInt(1)
  }
  when( io.incr ){
    cd := cd + UInt(1)
  }
  val wpAddr = (cd ## cp)

  // memory and register files
  val phb = Vec( ram.map( (i: Int) => Bool(i==1) ) )
  val index = RegNext( RegNext( phb( wpAddr ) ) ) // 2 cycles here // phb( wpAddr ) // 
  

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
  s1_out := Mux( io.s1, (op1 + op2), dat_out ) 

  dat_out := s1_out

  // write the data?

  // connect output ports
  io.xout := x_parr
  io.pout := dat_out
  //when( io.prst ){
    //io.pout := RegInit( UInt( 0, bitWidth ) )
  //}
  //------------------------------------------------------------------------------------//
  // datapath 2:
  val addrWidth = io.func.getWidth + log2Up(n/p)
  val res_out = RegInit( UInt(0, bitWidth) )
  //val hReg = ShiftRegister( io.hin, (n/p)-1 ) //****
  
  //val dataMem = Mem( UInt(width=bitWidth), 16 )
  val dataMem = Module( new PipelinedDualPortBRAM( addrWidth, bitWidth, 1, 2 ) ) // DualPortBRAM( 4, bitWidth ) )//

  // local ctrl signals
  val addr = RegInit( UInt( 0, width=log2Up(n/p) ) )
  when( io.ih ){
    if( n==p ){
      addr := addr
    }else{
      addr := addr + UInt(1)
    }  
  }
  val rAddr = (io.func ## addr)
  val opCode = ShiftRegister( io.func, 3+1 ) //RegInit( UInt(3, width=3) )
  //opCode := io.func
  //val rAddr = Reg( init=UInt( 0, 3+log2Up(n/p) ), next=wAddr )

  // initialise dataMem dual-port BRAM
  //when( io.init ){
  //  dataMem( rAddr ) := x_parr
  //}
  dataMem.io.ports(0).req.addr := rAddr
  dataMem.io.ports(0).req.writeData := x_parr
  dataMem.io.ports(0).req.writeEn := io.init

  

  // sum pass through lane
  val sReg = RegInit( UInt(0, width=bitWidth) )
  sReg := io.sin

  // operand 1
  // delay io.func by operand delay (4) - n/p + 1
  val passActive = ShiftRegister(io.func, 4 + 1 ) //Reg( init=UInt(0, width=3), next=io.func ) //ShiftRegister( opCode, n/p ) // initially when had is active, hrg=res_out, after n/p hreg=io.hin
  val hrg = MuxCase( res_out, Array(
                ( opCode === UInt(3, 3) || passActive === UInt(3,3) ) -> io.hin //&& passActive === UInt(3, 3) ) -> io.hin 
                ))
  val hReg = ShiftRegister( hrg, (n/p)-1 )
  /*
  val hReg2 = UInt(width=bitWidth)
  hReg2 := MuxCase( res_out, Array(
                  ( opCode === UInt(3, 3) ) -> io.hin 
                  ))
  val hReg = UInt(width=bitWidth)
  if( n==p ){
    hReg := hReg2
  } else{
    hReg := ShiftRegister( hReg2, (n/p)-1 )
  }*/


  // write, hon, rstG decode
  val hon = ( io.func === UInt(3,3) )
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
  //val gsak = RegInit( UInt(0, bitWidth) )
  val gsak = dataMem.io.ports(0).rsp.readData
  /*
  when( !io.init ){
    gsak := dataMem( rAddr )
  }*/
  

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

  //when( io.hfon){
  //  n_fix := n_fix + UInt(1)
  //}
  
  val hcg = RegInit( Bool(false) )
  val tmp = n_new & n_fix  // bitwise AND gate
  //hcg := xorR( tmp )

  // use a shift-register here (equal to BRAM pipeline registers)
  hcg := ShiftRegister( xorR(tmp), 3 )


  val op_had = UInt(width=bitWidth)
  op_had := Mux( hcg, -gsak, gsak )

  // sum register
  val sum = RegInit( UInt(0, width=bitWidth) )

  val yreg = RegInit( UInt(0, width=bitWidth) )
  val y_parr = RegInit( UInt(0, width=bitWidth) )
  y_parr := io.yin
  val y_data = ShiftRegister( y_parr, 2, en=io.load )
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
  //when( io.write ){
  //  dataMem( UInt(3,3) ## wAddr ) := res_out
  //}
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





  /*
  val inst_mem = Vec( Array(  UInt(0),
                              UInt(1),
                              UInt(2) ) )
  val pc = RegInit( init=UInt(0, width=4) )
  val pc_next = UInt()
  pc := pc_next
  pc_next := MuxCase( pc, Array(
                          (io.incr) -> pc + UInt(1) )

  val func = UInt()
  func := inst_mem( pc )
  */

