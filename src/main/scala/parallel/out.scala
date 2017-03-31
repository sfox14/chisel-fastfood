package parallel

import Chisel._
import utils._
import xilinx._

import math._

import scala.collection.mutable.Buffer


/*
Multiply-Add (i.e. dot-product) - Readout Layer 
*/
class OUT( val n_dicts : Int, val bitWidth : Int, val fracWidth : Int ) extends Module {

  val io = new Bundle{

    val dIn = Vec.fill( n_dicts ){ Fixed( INPUT, bitWidth, fracWidth ) }
    val dOut = Fixed( OUTPUT, bitWidth, fracWidth )
    val alpha = Vec.fill( n_dicts ){ Fixed( INPUT, bitWidth, fracWidth ) }

  }

  val prod = (0 until n_dicts).map( x => RegInit( Fixed( 0, width=bitWidth, fracWidth=fracWidth ) ) ) 

  // do multiply
  for (ix <- 0 until n_dicts){
    prod(ix) := RegNext( RegNext( RegNext( RegNext( io.alpha(ix) ) ) * RegNext( RegNext( io.dIn(ix) ) ) ) )
  }

  def reduceNums( numsToSum : Seq[Fixed] ) : ( Fixed, Int ) = {
    var stages = 0
    var tmpNums = numsToSum
    while( tmpNums.size > 1 ) {
      tmpNums = tmpNums.grouped(3).map( x => RegNext( x.reduce( _ + _ ) ) ).toSeq
      stages += 1
    }
    ( tmpNums(0), stages )
  }

  val (ypred, n_stages) = reduceNums( prod )

  val n_cycles = n_stages + 5 + 1 // 5 registers on multiply?

  io.dOut := RegNext( ypred )

}
