package parallel

import Chisel._
import scala.util.Random
import com.github.tototoshi.csv._
import utils._

// generate verilog
object gphbxVerilog {

  val n_features = 50
  val n_paths = 3
  val bitWidth = 18
  val fracWidth = 10

  // includes padding 
  val ram = List(1,1,0,1,0,1,0, 0,0)

  def main(args: Array[String]): Unit = {
    println("Generating verilog for GPHBx module")
    chiselMain(Array("--backend", "v", "--targetDir", "verilog"), 
                () => Module( new GPHBx( n_features, n_paths, bitWidth, fracWidth, 
                                        ram, BigInt(1), adderType.binAdd3, count.log2Up, 
                                        out.direct, false ) ) )
  }
}


// create a basic simulation
class gbhbxSim( c: GPHBx, X : List[List[BigInt]], verbose : Boolean = true ) extends Tester(c) {

  val rng = new Random( 23 )

  val d = c.n_features
  val k = c.n_paths

  val ram = c.ram

  printf("Dimension of input vector:  %d\n", d)
  printf("Number of data paths:       %d\n", k)
  printf("Length of LUT RAM:          %d\n", ram.length)


  // create some test data
  val ( dIn, dOut ) = preprocess.binary_gphbx(X, d, k, ram )
  //val ( dIn, dOut ) = preprocess.ternary_gphbx(X, d, k, ram )  
  //val ( dIn, dOut ) = preprocess.normal_gphbx(X, d, k, ram, c.G, c.fracWidth)

  //number of batches an input vector is divided into 
  val n_batches = dIn(0).length

  // number of examples
  val num = dIn.length

  // keep track of dOut
  var numI = 0

  // apply a reset  
  reset(20)

  for ( ix <- 0 until num ){

    for (iy <- 0 until n_batches){

      /*
      Introduce some random inValid patterns
      */
      var en = (rng.nextInt(5) >= 2)
      poke( c.io.dIn.valid, en )
      while ( !en ){
        // poke invalid random nums
        for( iz <- 0 until k){ poke( c.io.dIn.bits(iz), toFixed( rng.nextDouble, c.fracWidth ) ) }

        if (verbose){
          //peek( c.sreg.io.vld )
          //peek( c.sreg.io.out )
          peek( c.adderValid )
          peek( c.counter )
          peek( c.accReg )
        }
        
        step(1)

        if ( (peek(c.io.dOut.valid) == 1) && numI < num ){
          expect( c.io.dOut.bits, dOut(numI) )
          println( fromPeek.toDbl( peek(c.io.dOut.bits), c.bitWidth, c.fracWidth ) )
          numI += 1
        }

        en = (rng.nextInt(5) >= 2)
        poke( c.io.dIn.valid, en)

      }

      /*
      inValid == true, poke dIn chunks
      */
      for( iz <- 0 until k){ poke( c.io.dIn.bits(iz), dIn(ix)(iy)(iz) ) }

      if (verbose){
          //peek( c.sreg.io.vld )
          //peek( c.sreg.io.out )
          peek( c.adderValid )
          peek( c.counter )
          peek( c.accReg )
      }

      step(1)
 
      if ( (peek(c.io.dOut.valid) == 1) && numI < num ){
        expect( c.io.dOut.bits, dOut(numI) )
        println( fromPeek.toDbl( peek(c.io.dOut.bits), c.bitWidth, c.fracWidth ) )
        numI += 1
      }

    }
  }

  // evaluate remaining examples 
  for (ix <- 0 until c.aCycles+2){ //+2 -> for outFunc = mul
    
    if (verbose){
      peek( c.adderValid )
      peek( c.counter )
      peek( c.accReg )
    }

    step(1)
    
    if ( (peek(c.io.dOut.valid) == 1) && numI < num ){
        expect( c.io.dOut.bits, dOut(numI) )
        println( fromPeek.toDbl( peek(c.io.dOut.bits), c.bitWidth, c.fracWidth ) )
        numI += 1
      }
    
    if (verbose){
      peek( c.adderValid )
      peek( c.counter )
      peek( c.accReg )
    }

  }
  
}

object gbhbxTester {

  val n_dicts = 16
  val n_features = 8
  val n_paths = 2
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
  val ram = test.GPHB( false )(0).padTo( padding, 0 )
  println( ram, ram.length )
  val G = test.G( fracWidth )(0)

  // retrieve the X data for the dataset prescribed in py.make
  //val X = test.X( fracWidth )
  val X = random_data(10, n_features, fracWidth )


  def main(args: Array[String]): Unit = {
    println("Testing the GPHBx module")
    
    chiselMainTest(Array("--genHarness", "--test", "--backend", "c", 
      "--compile", "--targetDir", ".emulator"),
      () => Module(new GPHBx( n_features, n_paths, bitWidth, fracWidth, ram, G(0),
                               adderType.binAdd2, count.log2Up, out.direct ) ) ) {
        f => new gbhbxSim(f, X, true)
      }
  }

}
