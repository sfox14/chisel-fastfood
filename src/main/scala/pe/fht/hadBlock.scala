package fht

import Chisel._


// Hadamard block of PEs
//	- each hadamard contains p/(n/d) PEs, 
//	- a switching network, and
//	- global sum and dataIn routes

class HadBlock( val bitWidth : Int, val fracWidth : Int, val n : Int, 
	val p : Int, val d : Int, g : Seq[Seq[BigInt]], s : Seq[Seq[BigInt]],	
		alpha : Seq[Seq[BigInt]], val aStages : Int,
			forSim : Boolean = true ) extends Module{


	val io = new Bundle{
		val xin = Fixed(INPUT, bitWidth, fracWidth)
		val yin = Fixed(INPUT, bitWidth, fracWidth)
		val sin = Fixed(INPUT, bitWidth, fracWidth)

		val xout = Fixed(OUTPUT, bitWidth, fracWidth)
		val yout = Fixed(OUTPUT, bitWidth, fracWidth)
		val sout = Fixed(OUTPUT, bitWidth, fracWidth)

		val ctrl = new CtrlIO().flip()
		val ctrlOut = new CtrlIO()
	}

	// Dictionaries per PE, Hadamard Blocks and PEs per Hadamard
  	val k = n/p
  	val h = n/d
  	val b = p/h

	val block = (0 until b).reverse.map( i => Module( new PEfht(i, bitWidth, fracWidth,
											n, p, d, g(i), s(i), alpha(i), aStages, forSim) ) )

	val switch = Module( new HadSwitch( bitWidth, fracWidth, b, log2Up( b ) ) )

	// connect input/output data between PE's
	block(0).io.xin := io.xin
	block(0).io.yin := io.yin
	for (ix <- 1 until b){
		block(ix).io.xin := block(ix-1).io.xout
		block(ix).io.yin := block(ix-1).io.yout
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

	// pass data to next block
	io.xout := RegNext(io.xin)
	io.yout := RegNext(io.yin)
	io.ctrlOut := RegNext(io.ctrl)

	// *** dummy tester ***
	io.sout := block(0).io.yout + block(1).io.yout


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