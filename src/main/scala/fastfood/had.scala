package fastfood

import Chisel._
import utils._
import xilinx._

import scala.collection.mutable.Buffer

object reduceHadamard
{
  def apply( dIn : Buffer[Fixed], bitWidth : Int, fracWidth : Int, 
                zeros : List[Int] ) : Buffer[Buffer[Fixed]] = {

    var len = dIn.length
    var half = (len/2.0).toInt

    Predef.assert( (len & (len - 1)) == 0, "Error: Vector dimension is not a power of 2")

    var vdIn = dIn.grouped( half ).toVector
    var vdOut = (0 until len).map(x => RegInit( Fixed(0, bitWidth, fracWidth ) ) )
                .toBuffer.grouped(half).toBuffer 

    for (ix <- 0 until half){

      vdOut(0)(ix) := vdIn(0)(ix) + vdIn(1)(ix) 
      vdOut(1)(ix) := vdIn(0)(ix) - vdIn(1)(ix)
             
    }
    vdOut
  }

  def sparse( dIn : Buffer[(Fixed, Int)], bitWidth : Int, fracWidth : Int, 
                zeros : List[Int] ) : Buffer[Buffer[(Fixed,Int)]] = {

    var len = dIn.length
    var half = (len/2.0).toInt

    Predef.assert( (len & (len - 1)) == 0, "Error: Vector dimension is not a power of 2")

    var vdIn = dIn.grouped( half ).toVector
    var vdOut = (0 until len).map(x => ( Fixed(0, bitWidth, fracWidth ), 0 ) )
                .toBuffer.grouped(half).toBuffer 


    //var a = Buffer[((Fixed, Int), (Fixed,Int))]()

    var (op1, i1) = vdIn(0).unzip
    var (op2, i2) = vdIn(1).unzip

    for (ix <- 0 until half){

      /*
      If one or both of the indices are elements of the zeros list, we can reduce Hardware.
      The output index depends on whether the output is zero or not. If zero, we make sure
      the output index is on the zero list and vice versa. This ensures we can save resources
      if there are zeros on the next iteration.
      */
      if ( zeros.contains(i1(ix)) && zeros.contains(i2(ix)) ){
        vdOut(0)(ix) = ( RegNext( Fixed(0, bitWidth, fracWidth) ), i1(ix) )
        vdOut(1)(ix) = ( RegNext( Fixed(0, bitWidth, fracWidth) ), i1(ix) )
      } else if ( zeros.contains( i1(ix) ) ){
        vdOut(0)(ix) = ( RegNext( op2(ix)  ), i2(ix) )
        vdOut(1)(ix) = ( RegNext( -op2(ix) ), i2(ix) )
      } else if ( zeros.contains( i2(ix) ) ){
        vdOut(0)(ix) = ( RegNext( op1(ix) ), i1(ix) )
        vdOut(1)(ix) = ( RegNext( op1(ix) ), i1(ix) ) 
      } else{
        vdOut(0)(ix) = ( RegNext( op1(ix) + op2(ix) ), i1(ix) )
        vdOut(1)(ix) = ( RegNext( op1(ix) - op2(ix) ), i1(ix) )
      }
             
    }
    vdOut
  }



}


/*
Hadamard layer  - Parallel Processor
                - hadamard transform for a (n_features x n_features) matrix
                - for the full layer, containing n_dicts nodes, we need to do n_stack 
                  of these modules, where n_stack = n_dicts/n_features
                - takes advantage of structure in H matrix to implement with
                  min resources
*/
class HAD( val n_features : Int, val bitWidth : Int, val fracWidth : Int, 
            zeros : List[Int] = List[Int]() ) extends Module {

  val io = new Bundle{

    val dIn = Vec.fill( n_features ){ Fixed( INPUT, bitWidth, fracWidth ) }
    val dOut = Vec.fill( n_features ){ Fixed( OUTPUT, bitWidth, fracWidth ) } 

  }
  
  var n_cycles = log2Up( n_features )
  
  Predef.assert( (n_features & (n_features - 1)) == 0, "Error: n_features is not a power of 2")

  /*
  Hadamard Hardware Generator:
    - Uses mutable Buffers
    - Make dIn type: Buffer[Buffer[Fixed]], where the inner Buffer.length == n_features initially
    - Then, we recursively reduce the length of the inner Buffers, using reduceHadamard until == 1
  */

  //val hd = io.dIn.bits.toBuffer.grouped( feats ).toBuffer.map( (x:Buffer[Fixed]) => Buffer(x) )  
  
  var hd = Buffer( io.dIn.toBuffer.zipWithIndex ) //sparse
  //var hd = Buffer( io.dIn.toBuffer) //normal
  for (iy <- 0 until n_cycles){
    //hd = hd.map( x => reduceHadamard( x, bitWidth, fracWidth, zeros ) ).flatten
    hd = hd.map( x => reduceHadamard.sparse( x, bitWidth, fracWidth, zeros ) ).flatten
  }  
  val (hads, inds) = hd.flatten.unzip
  //val hads = hd.flatten

  println( hads, hads.length )


  for (ix <- 0 until n_features){
    io.dOut(ix) := hads(ix)
  }

}

// splits a list of {-1,0,1} to List(List(-1), List(+1))
// List(-1,1).map( x => g.zipWithIndex.collect {case (y, index) if (y == x) => index} )


/*
Could do sparse hadamard like this:

" eg. c = a.collect{ case ((x1,i1),(x2,i2)) => (x1,i2) } " // a is a Buffer/Vec/List

val a = (vdIn(0), vdIn(1)).zipped.toBuffer
Buffer[(Fixed, Int)] = a.collect{ case ( (x1, i1), (x2,i2) ) => 
                          if ( z.contains(i1) && z.contains(i2) ) (RegNext( Fixed(0, bw, fw) ), i1) }
                          ...
                          ...
for( ix <-0 until n){
  vdOut(0)(ix) = b0(ix)
  vdOut(1)(ix) = b1(ix)
}
*/