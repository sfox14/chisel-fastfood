package parallel

import Chisel._
import utils._
import xilinx._

import scala.collection.mutable.Buffer


/*
Reservoir layer   - Parallel Processor
                  - Implements a binary/ternary dot product from a List[Ints]
                    which specifies whether the respective input is accumulated
                    via addition, subtraction or avoided altogether
                  - Binary adds used (Still to do: Ternary adder function)
*/
class RES( val n_dicts : Int, val bitWidth : Int, val fracWidth : Int,
            val ram : List[Int] = List[Int]() ) extends Module {

  val io = new Bundle{

    val dIn = Vec.fill( n_dicts ){ Fixed( INPUT, bitWidth, fracWidth ) }
    val dOut = Fixed( OUTPUT, bitWidth, fracWidth ) 

  }
  
  Predef.assert( (n_dicts & (n_dicts - 1)) == 0, "Error: n_dicts is not a power of 2")

  // makes a List( List(neg indices), List(pos indices))
  var allNums = List(-1,1).map( x => ram.zipWithIndex.collect {case (y, index) if (y == x) => index} )
  

  // ternary adder tree
  def reduceNums( numsToSum : Seq[Fixed] ) : ( Fixed, Int ) = {
    var stages = 0
    var tmpNums = numsToSum
    while( tmpNums.size > 1 ) {
      tmpNums = tmpNums.grouped(3).map( x => RegNext( x.reduce( _ + _ ) ) ).toSeq
      stages += 1
    }
    ( tmpNums(0), stages )
  }

  var negNums = allNums(0).map( x => io.dIn(x) ).toSeq
  var posNums = allNums(1).map( x => io.dIn(x) ).toSeq

  var ( negNum, nStages ) = reduceNums( negNums )
  var ( posNum, pStages ) = reduceNums( posNums )

  // the number of cycles from valid input to valid output
  val nCycles = math.max( nStages, pStages ) + 1

  if (nStages == pStages){
    io.dOut := RegNext(posNum - negNum)
  } else if ( nStages < pStages){
    var delay = (pStages - nStages)
    io.dOut := RegNext( posNum - ShiftRegister(negNum, delay) )
  } else if ( nStages > pStages ){
    var delay = (nStages - pStages)
    io.dOut := RegNext( ShiftRegister(posNum, delay) - negNum )
  }

}
