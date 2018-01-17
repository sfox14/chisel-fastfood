package xilinx

import Chisel._
import utils._
import scala.util.Random

/*
Chisel Wrapper + Blackbox for FIFO IP generator core (uses BRAM) for 18-bit input
*/

class FIFO_BRAM extends BlackBox{
	val io = new Bundle{
		val din = UInt( INPUT, width=18 )
		val wr_en = Bool( INPUT )
		val rd_en = Bool( INPUT )
		val dout = UInt( OUTPUT, width=18 )
		val full = Bool( OUTPUT )
		val empty = Bool( OUTPUT )
		val count = UInt( OUTPUT, width=10 )

		def setNames(){
			din.setName("din")
			wr_en.setName("wr_en")
			rd_en.setName("rd_en")
			dout.setName("dout")
			full.setName("full")
			empty.setName("empty")
			count.setName("data_count")
		}
	}

	//io names map exactly to generated core
	io.setNames()
	renameClock( Driver.implicitClock, "clk" )
	renameReset( "rst" )
	setModuleName("fifo_18")

	// simulation
	val simFifo = Module( new Queue( UInt( width=18 ), 1024 ) )
	simFifo.io.enq.bits := io.din
	simFifo.io.enq.valid := io.wr_en
	simFifo.io.deq.ready := io.rd_en

	io.dout := RegNext( simFifo.io.deq.bits )
	io.full := RegNext( !simFifo.io.enq.ready )
	io.empty := RegNext( !simFifo.io.deq.valid )
	io.count := RegNext( simFifo.io.count)

}

// Chisel Wrapper
class FIFO[ T <: Data ]( genType : T ) extends Module{

	val io = new QueueIO( genType, 1024 ) // 1024 entries in BRAM

	Predef.assert( genType.getWidth() == 18, "Error: data width must be 18" )

	val din = UInt( width = 18).fromBits( io.enq.bits.flatten.map( x => x._2 ).reduceLeft( _##_ ) )
	val dout = UInt( width = 18 )
	dout := UInt(0)
	io.deq.bits := genType.fromBits( dout )

	val fifo_bram = Module( new FIFO_BRAM )

	fifo_bram.io.din := din
	fifo_bram.io.wr_en := io.enq.valid
	fifo_bram.io.rd_en := io.deq.ready

	dout := fifo_bram.io.dout
	io.deq.valid := !fifo_bram.io.empty
	io.enq.ready := !fifo_bram.io.full
	io.count := fifo_bram.io.count

}

// top module for testing fifo bram instantiation
class top extends Module{
	val io = new Bundle{
		val x = Decoupled( Fixed(INPUT, 18, 9) ).flip
		val y = Decoupled( Fixed(OUTPUT, 18, 9) )
	}

	val fifo = Module( new FIFO( Fixed(width=18, fracWidth=9) ) )
	val sum = RegInit( Fixed(0, width=18, fracWidth=9) )

	// connect fifo
	fifo.io.enq.bits := io.x.bits
	fifo.io.enq.valid := io.x.valid
	fifo.io.deq.ready := io.y.ready

	sum := io.x.bits + fifo.io.deq.bits
	io.x.ready := RegNext( fifo.io.enq.ready )
	io.y.valid := RegNext( fifo.io.deq.valid )
	io.y.bits := sum

}

class fifoSim( c : top ) extends Tester( c ){

	val rng = new Random(23)

	reset(10)
	poke( c.io.x.valid, true )
	poke( c.io.y.ready, true )

	for( ix <- 0 until 10 ){
		var inData = rng.nextFloat
		poke( c.io.x.bits, toFixed(inData, 9) )
		peek( c.io.y.valid )
		var outData = fromPeek.toDbl( peek( c.io.y.bits ), 18, 9 )
		println( s"xin: 	$inData" )
		println( s"yout: 	$outData")
		step(1)
	}
}

object fifoVerilog{

	def main(args: Array[String]): Unit = {
	println(s"Generating verilog for fifo bram")
	chiselMain(Array("--backend", "v", "--targetDir", "verilog"), 
		() => Module( new top ) )
	}

}

object fifoTester{


  def main(args: Array[String]): Unit = {
    println("Testing the fifo block")
    
    chiselMainTest(Array("--genHarness", "--test", "--backend", "c",
      "--compile", "--targetDir", ".emulator", "--vcd" ), 
      () => Module(new top ) ) {
        f => new fifoSim( f )
      }
  }
}
