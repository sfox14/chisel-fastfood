package hierarchical

import Chisel._
import utils._

// Hadamard block of PEs
//	- each hadamard contains p/(n/d) PEs, 
//	- a switching network, and
//	- global sum and dataIn routing

class HadBlock( val hid : Int, val bitWidth : Int, val fracWidth : Int, val n : Int, 
	val p : Int, val d : Int, g : Seq[Seq[BigInt]], s : Seq[Seq[BigInt]],	
		alpha : Seq[Seq[BigInt]], val aStages : Int,
			forSim : Boolean = true, eta_param : Double = 0.1 ) extends Module{


	val io = new Bundle{
		val xin = Fixed(INPUT, bitWidth, fracWidth)
		val xout = Fixed(OUTPUT, bitWidth, fracWidth)

		val ctrl = new CtrlIO().flip()
		val ctrlOut = new CtrlIO()

		// SumMod in HadBlock
		val yin = Fixed(INPUT, bitWidth, fracWidth)
		val sin = Fixed(INPUT, bitWidth, fracWidth)
		val yout = Fixed(OUTPUT, bitWidth, fracWidth)
		val sout = Fixed(OUTPUT, bitWidth, fracWidth)
	}

	// Dictionaries per PE, Hadamard Blocks and PEs per Hadamard
  	val k = n/p
  	val h = n/d
  	val b = p/h

	val block = (0 until b).reverse.map( i => Module( new PE(i, hid, bitWidth, fracWidth,
								n, p, d, g(i), s(i), alpha(i), aStages, forSim) ) )

	val switch = Module( new HadSwitch( bitWidth, fracWidth, b, log2Up( b ) ) )

	// Connect input/output data between PE's
	block(0).io.xin := io.xin
	for (ix <- 1 until b){
		block(ix).io.xin := block(ix-1).io.xout
	}

	// broadcast ctrl signal to PEs
	for( ix <- 0 until b ){
		block(ix).io.ctrl <> io.ctrl.pe
	}

	// connect hin/hout to switching network, (and reorder)
	for( ix <- 0 until b ){
		switch.io.hin( ix ) := block( b-1-ix ).io.hout
		block( b-1-ix ).io.hin := switch.io.hout( ix )
	}

	//-------------------------- CONNECT SumMod ---------------------------------//
	
	val sumMod = Module( new SumMod( bitWidth, fracWidth, aStages, b, eta_param, forSim ) )

	sumMod.io.gsin := io.sin
	sumMod.io.func := io.ctrl.pe.func
	sumMod.io.yload := io.ctrl.pe.yload
	sumMod.io.yin := io.yin
	for( ix <- 0 until b){
		sumMod.io.sin( ix ) := block( ix ).io.sout
		block( ix ).io.delta := sumMod.io.delta
	}

	// pass data to next block
	io.xout := RegNext(io.xin)
	io.sout := sumMod.io.gsout
	io.yout := sumMod.io.yout
	io.ctrlOut := RegNext(io.ctrl)

}

class HadSwitch( val bitWidth : Int, val fracWidth : Int, b : Int, lvl : Int  ) extends Module{

	val io = new Bundle{
		val hin = Vec.fill( b ){ Fixed(INPUT, bitWidth, fracWidth) }
		val hout = Vec.fill( b ){ Vec.fill( lvl ){ Fixed(OUTPUT, bitWidth, fracWidth) } }

	}

	// generate switching pattern for hadamard
	val patt = (0 until b).map( x => (0 until lvl).map( y => (1<<y) ^ x ) )
	println("Hadamard Switch: ")
	println(patt)

	// hard code switching
	for( ix <- 0 until b ){
		for( iy <- 0 until lvl ){
			io.hout( ix )( iy ) := io.hin( patt(ix)(iy) )
		}
	}
}

class SumMod( val bitWidth : Int, val fracWidth : Int, val aStages : Int,
						val b : Int, eta_param : Double = 0.1, forSim : Boolean = true ) extends Module {
	val io = new Bundle{
		val sin = Vec.fill( b ){ Fixed(INPUT, bitWidth, fracWidth) }
		val gsin = Fixed( INPUT, bitWidth, fracWidth )
		val yin = Fixed( INPUT, bitWidth, fracWidth )
		val func = UInt(INPUT, width=4)
		val yload = Bool(INPUT)
		
		val delta = Fixed(OUTPUT, bitWidth, fracWidth)
		val gsout = Fixed(OUTPUT, bitWidth, fracWidth )
		val yout = Fixed(OUTPUT, bitWidth, fracWidth)	
	}

	/*
	This module computes the block sum, global sum, and prepares the update
	*/

	val mStages = 4

	// register control signal
	val yload = Reg( init=Bool(false), next=io.yload )
	val func = Reg( init=UInt(0, 4), next=io.func )
	val opCode = ShiftRegister( func, mStages + 1 )
	val aluCode = ShiftRegister( opCode, aStages + 1 )

	// application registers
	val sreg = RegInit( Fixed(0, bitWidth, fracWidth) )
	val sum_local = RegInit( Fixed(0, bitWidth, fracWidth) )
	val error = RegInit( Fixed(0, bitWidth, fracWidth) )
	val delta = RegInit( Fixed(0, bitWidth, fracWidth) )
	val eta = RegInit( Fixed( toFixed(-eta_param, fracWidth), bitWidth, fracWidth ) )
	sreg := io.gsin


	  // y register for computing the error
	val yreg = RegInit( Fixed(0, width=bitWidth, fracWidth=fracWidth) )
	val y_parr = RegInit( Fixed(0, width=bitWidth, fracWidth=fracWidth) )
	y_parr := io.yin
	when( yload ){
		yreg := -y_parr	
	}
	error := ( sreg + yreg )
	

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


	// adder tree
	def reduceNums( numsToSum : Seq[Fixed] ) : ( Fixed, Int ) = {
		var stages = 0
		var tmpNums = numsToSum
		while( tmpNums.size > 1 ) {
		  tmpNums = tmpNums.grouped(2).map( x => RegNext( x.reduce( _ + _ ) ) ).toSeq
		  stages += 1
		}
		( tmpNums(0), stages )
	}
	val ( sum, nStages) = reduceNums( io.sin )
	when( aluCode === UInt(7) ){
		sum_local := sum
	}

	// reset global sum
	when( aluCode === UInt(14) ){ //14
		sreg := Fixed(0, bitWidth, fracWidth)
	}

	///*
	when( opCode === UInt(15) ){  //15
		error := Fixed(0, bitWidth, fracWidth)
	}
	//*/
	//delta := dspMultiply( error, eta, 1, 2 ) // verilog
	///*
	when( opCode =/= UInt(13) ){
		delta := dspMultiply( error, eta, 1, 2 )
	}
	//*/

	io.delta := delta
	io.yout := y_parr

	io.gsout := RegNext(sreg + sum_local) // verilog
	if( forSim ){
		io.gsout := (sreg + sum_local) // simulation
	}
	
}