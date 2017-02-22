package utils

import Chisel._
import scala.math._


object log3Up
{
  def apply(in: Int): Int = if(in == 1) 1 else ceil( log10( in.toFloat )/log10( 3.0 ) ).toInt
}

object log4Up
{
  def apply(in: Int): Int = if(in == 1) 1 else ceil( log10( in.toFloat )/log10( 4.0 ) ).toInt
}

object adderType
{
  def triAdd3( a : Fixed, b: Fixed, c: Fixed ): Fixed = {
  	Reg( next = a + b + c )
  }

  def triAdd4( a : Fixed, b: Fixed, c: Fixed, d: Fixed ): Fixed = {
  	RegNext( RegNext( a + b + c ) + RegNext( d ) )
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