package utils

import Chisel._
import scala.math._
import scala.util.Random
import sys.process._
import com.github.tototoshi.csv._
import scala.language.postfixOps


object count
{
  def log2Up(in: Int): Int = if(in == 1) 1 else ceil( log10( in.toFloat )/log10( 2.0 ) ).toInt

  def log3Up(in: Int): Int = if(in == 1) 1 else ceil( log10( in.toFloat )/log10( 3.0 ) ).toInt

  def log4Up(in: Int): Int = if(in == 1) 1 else ceil( log10( in.toFloat )/log10( 4.0 ) ).toInt
}



object toFixed
{
  def apply( myDouble : Double, fracWidth : Int, message : String = "Error: could not convert to fixed" ) : BigInt = {
    try{
        ( myDouble * BigDecimal( BigInt(1) << fracWidth ) ).toBigInt
      } catch{
        case x:Exception => throw new Exception(message)
      }
  }
}


object fromPeek
{
  def apply( myNum : BigInt, bitWidth : Int ): BigInt = {
    if (myNum >= (BigInt(1) << (bitWidth - 1)))
      myNum - (BigInt(1) << bitWidth)
    else
      myNum
  }

  def toDbl( myNum : BigInt, bitWidth : Int, fracWidth : Int): Double = {

    val x = apply( myNum, bitWidth ).toDouble
    val ONE = (BigInt(1) << fracWidth).toDouble
    x/ONE
  } 
}

// list of [n_examples x n_features] between -1 and 1
object random_data
{
  def apply( n_examples : Int, n_features : Int, fracWidth : Int,
             rng : Random = new Random(43) ) : List[List[BigInt]] = {
    (0 until n_examples).map( x => (0 until n_features)
      .map( y => toFixed(2*rng.nextDouble -1, fracWidth) )
      .toList ).toList
  }
}

/*
used to preprocess data for simulation. ensures that each input vector is split
into batches with correct padding applied. also calculates output for a task. 
*/
object preprocess
{

  def transform_x( X : List[List[BigInt]], n_features : Int, n_paths: Int ) = {

    val n_batches = math.ceil(n_features/n_paths.toFloat).toInt
    val padding = n_batches*n_paths

    X.map(x=> x.padTo(padding, BigInt(0)).toList.grouped( n_paths ).toList)

  }

  def binary_gphbx( X : List[List[BigInt]], n_features : Int, n_paths: Int, 
                    ram : List[Int] ) = {

    val dIn = transform_x( X, n_features, n_paths ) 
    
    var dInTemp = dIn.map(x => x.flatten )
    var dOutTemp = dInTemp.map( x => (ram.map(x => x*2 -1), x)
                    .zipped.map(_*_) )
                    .map(x => x.grouped(n_paths)
                    .toList)
    var dOut = dOutTemp.map(x => x.map(y => y.reduce(_+_)).reduce(_+_) ).toList

    (dIn, dOut)

  }

  def ternary_gphbx( X : List[List[BigInt]], n_features : Int, n_paths: Int, 
                    ram : List[Int] ) = {

    //val tram = ram.grouped(2).map(x => if  )
    val tram = ram.grouped(2).toList
                .map(x => x.reduce(_+_))
                .map(y => if (y==1) -1 else if (y==2) 1 else 0 )

    val dIn = transform_x( X, n_features, n_paths )
    
    var dInTemp = dIn.map(x => x.flatten )
    
    Predef.assert( tram.length == dInTemp(0).length, "Error: input length doesn't match ram" )

    var dOutTemp = dInTemp.map( x => (tram, x).zipped.map(_*_) ).map(x => x.grouped(n_paths).toList)
    var dOut = dOutTemp.map(x => x.map(y => y.reduce(_+_)).reduce(_+_) ).toList

    (dIn, dOut)

  }

  def normal_gphbx( X : List[List[BigInt]], n_features : Int, n_paths: Int, 
                    ram : List[Int], G : BigInt, fracWidth : Int ) = {

    val (dIn, dO) = binary_gphbx( X, n_features, n_paths, ram )
    val dOut = dO.map( x => x*G >> fracWidth )

    (dIn, dOut)

  } 

}



class FastFoodTest( var n_dicts : Int, var n_features : Int, 
                    var gType : String = "binary", var n_examples : Int = 10 )
{
  /*
  B.csv H.csv S.csv G.csv PHB.csv GPHB.csv
  */

  def openCSV( filename : String ) : List[List[String]] = {
    
    val reader = CSVReader.open( new java.io.File( filename ) )
    val data = reader.all()
    reader.close()
    data

  }

  // this is where the data is made
  // could accept a dataset param, an rng param, and n_examples param
  def make() = {

    // runs the python script for certain parameters
    var g = 1
    if (gType == "binary"){
      g = 1
    } else if (gType == "ternary"){
      g = 2
    } else if (gType == "normal"){
      g = 0
    } else{
      throw new Exception("select either binary/ternary/normal")
    }

    var cmd = "python src/main/python/make_params.py "+
              f"$n_dicts $n_features $g /.tmp"
    cmd !

  }

  def S( fracWidth : Int ): List[List[BigInt]] = {
    try{
        openCSV( ".tmp/S.csv" ).map(x => x.map(y => toFixed(y.trim.toDouble, fracWidth)))
      } catch{
          case x:Exception => throw new Exception("Error: .tmp/S.csv")
      }
    
  }

  def B(): List[List[Int]] = {
    try{
        openCSV( ".tmp/B.csv" ).map(x => x.map(y => ((y.toDouble+1)/2.0).toInt ) )
      } catch{
          case x:Exception => throw new Exception("Error: .tmp/B.csv")
      }
  }

  def H(): List[List[Int]] = {
    try{
        openCSV( ".tmp/H.csv" ).map(x => x.map(y => ((y.toDouble+1)/2.0).toInt ) )
      } catch{
          case x:Exception => throw new Exception("Error: .tmp/H.csv")
      }
  }

  def G( fracWidth : Int ): List[List[BigInt]] = {
    try{
        openCSV( ".tmp/G.csv" ).map(x => x.map(y => toFixed(y.trim.toDouble, fracWidth)))
      } catch{
          case x:Exception => throw new Exception("Error: .tmp/G.csv")
      }
  }

  def GPHB( ternary : Boolean = false ): List[List[Int]] = {
    // to 1 and zero
    if ( ternary ){
      // converts -1,0,+1 to 0,1 but twice the length
      // 00 = 0, 01 = -1, 11 = +1

      // *TO DO:
      //1. if all equal 0 - exit with error
      //2. if none equal 0 - just do binarised
      //3. else - to ternary 

      openCSV( ".tmp/GPHB.csv" )
        .map(x => x.map(y => if (y.toDouble.toInt==0) List(0,0) else List((y.toDouble+1/2.0).toInt,1) )
        .flatten)

    } else{
      // converts -1, +1 to 0 and 1
      openCSV( ".tmp/GPHB.csv" )
      .map(x => x.map(y => ((y.toDouble+1)/2.0).toInt ) )
    } 
  }

  def PHB(): List[List[Int]] = {
    try{
        openCSV( ".tmp/PHB.csv" ).map(x => x.map(y => ((y.toDouble+1)/2.0).toInt ) )
      } catch{
          case x:Exception => throw new Exception("Error: .tmp/PHB.csv")
      }
  }

  def X( fracWidth : Int ): List[List[BigInt]] = {
    try{
        openCSV( ".tmp/X.csv" ).map(x => x.map(y => toFixed(y.trim.toDouble, fracWidth)))
      } catch{
          case x:Exception => throw new Exception("Error: .tmp/X.csv")
      }
  }

}