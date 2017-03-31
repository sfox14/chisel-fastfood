package parallel

import Chisel._
import scala.util.Random
import com.github.tototoshi.csv._
import utils._

import scala.collection.mutable.Buffer 

// generate verilog
object trainVerilog {

  val n_dicts = 128
  val n_features = 64
  val n_paths = 3
  val bitWidth = 18
  val fracWidth = 10


  // number of batches required for an input vector
  val n_batches = math.ceil(n_features/n_paths.toFloat).toInt
  var padding = n_batches*n_paths

  val gType = "binary"

    // create a fastfood python-dependent test object 
  val test = new FastFoodTest( n_dicts, n_features, gType )
  test.make

  // retrieve architecture specific parameters, LSB to MSB
  val ram = test.GPHB( false ).map( x => x.padTo( padding, 0 ) )
  println( ram, ram.length, ram(0).length )
  val G = test.G( fracWidth ).flatten
  println( G )
  val S = test.S( fracWidth ).flatten
  println( S )
  val U = test.U().flatten
  println( U )

  val alpha = test.ALPHA().flatten
  println( alpha )


  def main(args: Array[String]): Unit = {
    println("Generating verilog for Kernel Expansion module")
    chiselMain(Array("--backend", "v", "--targetDir", "verilog"), 
                () => Module( new TRAIN( n_dicts, n_features, n_paths, bitWidth, fracWidth, 
                                        ram, G, S, U, alpha ) ) )
  }
}


// create a basic simulation
class trainSim( c: TRAIN, X : List[List[BigInt]], verbose : Boolean = true ) extends Tester(c) {

  val rng = new Random( 23 )

  //val f = new java.io.File( "chiselFF_train.csv" )
  //val writer = CSVWriter.open(f)

  val n = c.n_dicts
  val d = c.n_features
  val k = c.n_paths

  val g = c.g
  val ram = c.ram

  printf("Number of dicts:            %d\n", n)
  printf("Dimension of input vector:  %d\n", d)
  printf("Number of data paths:       %d\n", k)

  // create some test data
  //val ( dIn, dOut ) = preprocess.binary_gphbx(X, d, k, ram )

  val dIn = preprocess.transform_x( X, d, k )

  //number of batches an input vector is divided into 
  val n_batches = dIn(0).length
  println( n_batches )

  // number of examples
  val num = dIn.length
  println( num )
  
  // keep track of dOut
  var numI = 0

  // apply a reset  
  reset(20)

  for ( ix <- 0 until num ){

    for (iy <- 0 until n_batches){

      /*
      Introduce some random inValid patterns
      */
      var en = (rng.nextInt(5) >= 0)
      poke( c.io.dIn.valid, en )
      while ( !en ){
        // poke invalid random nums
        for( iz <- 0 until k){ poke( c.io.dIn.bits(iz), toFixed( rng.nextDouble, c.fracWidth ) ) }
        
        step(1)

        if ( (peek(c.io.dOut.valid) == 1) && numI < num ){
          println( fromPeek.toDbl( peek(c.io.dOut.bits), c.bitWidth, c.fracWidth ) )
          numI += 1

          //var a = Buffer[Double]()
          //for ( iv <- 0 until n ){
            //a += ( fromPeek.toDbl( peek(c.io.dOut.bits(iv)), c.bitWidth, c.fracWidth ) )
            //a += b
            //println(b)
          //}
          //writer.writeRow( a )    
        }

        en = (rng.nextInt(5) >= 2)
        poke( c.io.dIn.valid, en)

      }

      /*
      inValid == true, poke dIn chunks
      */
      for( iz <- 0 until k){ poke( c.io.dIn.bits(iz), dIn(ix)(iy)(iz) ) }

      step(1)
 
      if ( (peek(c.io.dOut.valid) == 1) && numI < num ){
        println( fromPeek.toDbl( peek(c.io.dOut.bits), c.bitWidth, c.fracWidth ) )
        numI += 1

        //var a = Buffer[Double]()
        //for ( iv <- 0 until n ){
          //a += ( fromPeek.toDbl( peek(c.io.dOut.bits(iv)), c.bitWidth, c.fracWidth ) )
          //a += b
          //println(b)
        //}
        //writer.writeRow( a ) 

      }

    }
  }

  poke( c.io.dIn.valid, false )
  // evaluate remaining examples 
  while ( numI < num ){ //+2 -> for outFunc = mul
    
    step(1)
    
    if ( (peek(c.io.dOut.valid) == 1) && numI < num ){
        println( fromPeek.toDbl( peek(c.io.dOut.bits), c.bitWidth, c.fracWidth ) )
        numI += 1

        //var a = Buffer[Double]()
        //for ( iv <- 0 until n ){
          //a += ( fromPeek.toDbl( peek(c.io.dOut.bits(iv)), c.bitWidth, c.fracWidth ) )
          //a += b
          //println(b)
        //}
        //writer.writeRow( a )

      }
    peek( c.vldUpdate )

  }
  //writer.close()
}

object trainTester {

  val n_dicts = 128
  val n_features = 64
  val n_paths = 3
  val bitWidth = 18
  val fracWidth = 10

  // number of batches required for an input vector
  val n_batches = math.ceil(n_features/n_paths.toFloat).toInt
  var padding = n_batches*n_paths

  // type of gaussian matrix
  val gType = "binary"
  if ( gType == "ternary" ){
    padding = padding*2
  }

  // create a fastfood python-dependent test object 
  val test = new FastFoodTest( n_dicts, n_features, gType )
  test.make

  // retrieve architecture specific parameters, LSB to MSB
  val ram = test.GPHB( false ).map( x => x.padTo( padding, 0 ) )
  println( ram, ram.length, ram(0).length )
  val G = test.G( fracWidth ).flatten
  println( G )
  val S = test.S( fracWidth ).flatten
  println( S )
  val U = test.U().flatten
  println( U )

  val alpha = test.ALPHA().flatten
  println( alpha )

  // retrieve the X data for the dataset prescribed in py.make
  val X = test.X( fracWidth ) //.map( x=> x.padTo(32, BigInt(0)) )
  println(X, X.length)


  def main(args: Array[String]): Unit = {
    println("Testing the Kernel Expansion module")
    
    chiselMainTest(Array("--genHarness", "--test", "--backend", "c", //"--wio", 
      "--compile", "--targetDir", ".emulator"), // .emulator is a hidden directory
      () => Module(new TRAIN( n_dicts, n_features, n_paths, bitWidth, fracWidth, 
                              ram, G, S, U, alpha ) ) ) {
        f => new trainSim(f, X, true)
      }
  }

}
