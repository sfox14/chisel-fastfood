package utils

import Chisel._
import scala.math._
import scala.util.Random

import xilinx._

object adderType
{
  def triAdd3( a : Vector[Fixed] ): Fixed = {
  	RegNext( next = a(0) + a(1) + a(2) )
  }

  def triAdd4( a : Vector[Fixed] ): Fixed = {
  	RegNext( RegNext( a(0) + a(1) + a(2) ) + RegNext( a(3) ) )
  }

  def binAdd2( a : Vector[Fixed] ): Fixed = {
  	RegNext( a(0) + a(1) )
  } 

  def binAdd3( a : Vector[Fixed] ): Fixed = {
  	RegNext( RegNext( a(0) + a(1) ) + RegNext( a(2) ) )
  }

  def binAdd4( a: Vector[Fixed] ): Fixed = {
  	RegNext( RegNext( a(0) + a(1) ) + RegNext( a(2) + a(3) ) )
  }
}

// gphb functions (binary or ternary)
object stream
{
  def binary( dIn : DecoupledIO[Vec[Fixed]], n_paths : Int,
                  sr : List[Int] ) : Vector[Fixed] = {


    val sreg = Module( new LUTSR32( sr, n_paths ) )
    sreg.io.vld := dIn.valid
    
    (0 until n_paths).map(x => Mux( sreg.io.out(x), dIn.bits(x), -dIn.bits(x) ) ).toVector

  }
  
  def ternary( dIn : DecoupledIO[Vec[Fixed]], n_paths : Int,
                  sr : List[Int], forSim : Boolean = true ) : Vector[Fixed] = {

    val sreg = Module( new LUTSR32( sr, 2*n_paths ) )
    sreg.io.vld := dIn.valid
    val sel = sreg.io.out.toVector.grouped(2).toVector
    
    val zero = dIn.bits(0).cloneType.unary_-

    // two select lines. bit(0) for {-1,+1}, bit(1) for {0, !=0}
    (0 until n_paths)
      .map(x => Mux( sel(x)(1), 
                  Mux( sel(x)(0), dIn.bits(x), -dIn.bits(x) ),
                    zero ) ).toVector

  }

}

object out
{
	def direct( in : Fixed, w : Fixed, vld : Bool): (Fixed, Bool) = {
		(in, vld)
	}

	def mul( in : Fixed, w : Fixed, vld : Bool): (Fixed, Bool) = {
		( RegNext( RegNext( in ) * RegNext( w ) ), RegNext( RegNext( vld ) ) )
	}
}
