package fastfood

import Chisel._
import scala.util.Random
import com.github.tototoshi.csv._
import utils._

// generate verilog
object gphbxVerilog {

  val d = 50
  val k = 3
  val bitWidth = 18
  val fracWidth = 10

  // includes padding 
  val SR = List(1,1,0,1,0,1,0, 0,0)

  def main(args: Array[String]): Unit = {
    println("Generating verilog for GPHBx module")
    chiselMain(Array("--backend", "v", "--targetDir", "verilog"), 
                () => Module( new GPHBx( d, k, SR, adderType.binAdd3, bitWidth, fracWidth, false ) ) ) 

  }
}


// create a basic simulation
class gbhbxSim( c: GPHBx, verbose : Boolean = true ) extends Tester(c) {

  
  def toFixed( myFloat : Float, message : String ) : BigInt = {
    try{
        ( myFloat * BigDecimal( BigInt(1) << c.fracWidth ) ).toBigInt
      } catch{
        case x:Exception => throw new Exception(message)
      }
  }

  def fromPeek(myNum : BigInt) : BigInt = {
    if (myNum >= (BigInt(1) << (c.bitWidth - 1)))
      myNum - (BigInt(1) << c.bitWidth)
    else
      myNum
  }


  val myRand = new Random
  val num = 10
  val d = c.d
  val k = c.k

  val sr = c.SR.map(x => x*2 -1 )

  printf("Dimension of input vector:  %d\n", d)
  printf("Number of data paths:       %d\n", k)

  // create some test data
  val chunks = math.ceil(d/k.toFloat).toInt
  val pd = chunks*k

  val dIn = (0 until num).map(x => (0 until d).map(x => toFixed( myRand.nextFloat, "error" ) ).padTo(chunks*k, BigInt(0)).toList.grouped( k ).toList).toList
  val dInTemp = dIn.map(x => x.flatten )
  val dOutTemp = dInTemp.map( x => (sr, x).zipped.map(_*_) ).map(x => x.grouped(k).toList)
  val dOut = dOutTemp.map(x => x.map(y => y.reduce(_+_)).reduce(_+_) ).toList
  
  // timing parameters
  val iCycles = c.iCycles
  val aCycles = c.aCycles
  val nCycles = c.nCycles

  // keep track of dOut
  var numI = 0

  // apply a reset  
  reset(20)

  for ( ix <- 0 until num ){

    for (iy <- 0 until chunks){

      /*
      Introduce some random inValid patterns
      */
      var en = (myRand.nextInt(5) >= 2)
      poke( c.io.dIn.valid, en )
      while ( !en ){
        // poke invalid random nums
        for( iz <- 0 until k){ poke( c.io.dIn.bits(iz), toFixed( myRand.nextFloat, "error" ) ) }

        if (verbose){
          peek( c.sreg.io.vld )
          peek( c.sreg.io.out )
          peek( c.adderValid )
          peek( c.counter )
          peek( c.accReg )
        }
        
        step(1)

        if ( (peek(c.io.dOut.valid) == 1) && numI < num ){
          expect( c.io.dOut.bits, dOut(numI) )
          numI += 1
        }

        en = (myRand.nextInt(5) >= 2)
        poke( c.io.dIn.valid, en)

      }

      /*
      inValid == true, poke dIn chunks
      */
      for( iz <- 0 until k){ poke( c.io.dIn.bits(iz), dIn(ix)(iy)(iz) ) }

      if (verbose){
          peek( c.sreg.io.vld )
          peek( c.sreg.io.out )
          peek( c.adderValid )
          peek( c.counter )
          peek( c.accReg )
      }

      step(1)
 
      if ( (peek(c.io.dOut.valid) == 1) && numI < num ){
        expect( c.io.dOut.bits, dOut(numI) )
        numI += 1
      }

    }
  }

  // evaluate remaining examples 
  for (ix <- 0 until aCycles){
    
    if (verbose){
      peek( c.adderValid )
      peek( c.counter )
      peek( c.accReg )
    }

    step(1)
    
    if ( (peek(c.io.dOut.valid) == 1) && numI < num ){
        expect( c.io.dOut.bits, dOut(numI) )
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

  val d = 7
  val k = 3
  val bitWidth = 18
  val fracWidth = 10

  // includes padding 
  val SR = List(1,1,0,1,0,1,0, 0,0) //LSB to MSB

  def main(args: Array[String]): Unit = {
    println("Testing the GPHBx module")
    
    chiselMainTest(Array("--genHarness", "--test", "--backend", "c", //"--wio", 
      "--compile", "--targetDir", ".emulator"), // .emulator is a hidden directory
      () => Module(new GPHBx( d, k, SR, adderType.binAdd3, bitWidth, fracWidth ) ) ) {
        f => new gbhbxSim(f, true)
      }
  }

}
