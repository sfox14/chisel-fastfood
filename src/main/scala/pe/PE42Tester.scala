package pe

import Chisel._
import com.github.tototoshi.csv._
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.util.Random

import utils._

import math._

class pe42Sim( c: Array42 ) extends Tester( c ){

  val ram = c.ram
  val rsd = new Random( 21 )

  val n = c.n   // Neurons/dicts
  val p = c.p   // PEs
  val d = c.d   // x dim
  val k = n/p  // neurons per PE

  poke(c.io.s1, false)
  poke(c.io.load, false)
  poke(c.io.write, false)
  poke(c.io.incr, false)  // we start counting ram addr two cycles
  poke(c.io.init, false)
  poke(c.io.ih, false)
  poke(c.io.hon, false)
  poke(c.io.hrst, false)
  poke(c.io.rstG, false)

  def initG(){
    // initialise BRAM
    poke(c.io.func, 0) // g
    for( iy <- 0 until k ){
      for( ix <- 0 until p ){
        poke( c.io.xin, BigInt( rsd.nextInt(5) )) // g1
        step(1)
      }
      poke(c.io.init, true)
      poke(c.io.ih, true)
      step(1)
      poke(c.io.init, false)
      poke(c.io.ih, false)
      step(1)
    }
  }

  def initS(){
    // initialise BRAM
    poke(c.io.func, 1) // g
    for( iy <- 0 until k ){
      for( ix <- 0 until p ){
        poke( c.io.xin, BigInt( rsd.nextInt(5) )) // g1
        step(1)
      }
      poke(c.io.init, true)
      poke(c.io.ih, true)
      step(1)
      poke(c.io.init, false)
      poke(c.io.ih, false)
      step(1)
    }
  }

  def initA(){
    // initialise BRAM
    poke(c.io.func, 2) // g
    for( iy <- 0 until k ){
      for( ix <- 0 until p ){
        poke( c.io.xin, BigInt( rsd.nextInt(5) )) // g1
        step(1)
      }
      poke(c.io.init, true)
      poke(c.io.ih, true)
      step(1)
      poke(c.io.init, false)
      poke(c.io.ih, false)
      step(1)
    }
  }

  initG()
  initS()
  initA()

  poke(c.io.func, 7)
  val ya = BigInt(1022)
  poke(c.io.yin, ya)
  
  val rng = new Random( 33 )

  // create one input vector
  //val xin = (0 until d).map(x => BigInt( rng.nextInt(10) ))

  //phbx()
  pe1682()
  //pe842()

  // phbx tester (works for N/D/P)
  def phbx(){
  
    for( ix <- 0 until p-1 ){
        poke(c.io.xin, BigInt( rng.nextInt(10) ))
        step(1)
    }
    poke(c.io.xin, BigInt( rng.nextInt(10) )) // last xin
    poke(c.io.proc, true) // start getting wpAddr
    step(1)

    val nsp = n-p
    val n2 = n-2
    
    var gap = n2-nsp
    var vp = p

    // case 1: nsp == 0
    // case 2: nsp <= n2
    // invalid cases: n2<nsp, n=2, p=2

    println( gap, vp )

    poke(c.io.load, true)
    if ( nsp == 0 ){ 
      poke(c.io.xin, BigInt( rng.nextInt(10) ))
      gap -= 1
      vp -= 1
    }
    step(1)
    poke(c.io.load, false)
    poke(c.io.s1, true)

    for( iy <- 0 until (d/p)-1 ){

      if( nsp>1 ){ step( nsp-1 ) }
      
      while( gap > 0 ){
        poke(c.io.xin, BigInt( rng.nextInt(10) ))
        gap -= 1
        vp -= 1
        step(1)
      }

      poke(c.io.incr, true)
      poke(c.io.xin, BigInt( rng.nextInt(10) ))
      vp -= 1
      step(1)
      poke(c.io.incr, false)

      while( vp > 0 ){
        poke(c.io.xin, BigInt( rng.nextInt(10) ))
        vp -= 1
        step(1)
      }

      vp = p
      gap = n2 - nsp

      println( gap, vp )

      poke(c.io.load, true)
      if ( nsp == 0 ){ 
        poke(c.io.xin, BigInt( rng.nextInt(10) ))
        gap -= 1
        vp -= 1
      }
      step(1)
      poke(c.io.load, false)    
    }

    step( n-2-1 )

    poke(c.io.incr, true)
    step(1)
    poke(c.io.incr, false)
    poke(c.io.proc, false)
    step(2) // 2 cycles for the read from Mem
    poke(c.io.s1, false)

    step(1)
    poke(c.io.func, 7)

      // 4. finish
    for(ix <- 0 until 10){
      step(1)
    }
  }

  // main dev version (will only work for n-n/p-II>0, i.e. n>=16, and n/p <= d)
  def pe1682(){

    // starting loading data and computing phbx
    for( ix <- 0 until p-1 ){
        poke(c.io.xin, BigInt( rng.nextInt(10) ))
        step(1)
      }
    poke(c.io.xin, BigInt( rng.nextInt(10) )) // last xin
    poke(c.io.proc, true) // start getting wpAddr
    step(1)

    val nsp = n-p
    val n2 = n-2
    
    var gap = n2-nsp
    var vp = p

    // case 1: nsp == 0
    // case 2: nsp <= n2
    // invalid cases: n2<nsp, n=2, p=2

    println( gap, vp )

    poke(c.io.load, true)
    if ( nsp == 0 ){ 
      poke(c.io.xin, BigInt( rng.nextInt(10) ))
      gap -= 1
      vp -= 1
    }
    step(1)
    poke(c.io.load, false)
    poke(c.io.s1, true)

    for( iy <- 0 until (d/p)-1 ){

      if( nsp>1 ){ step( nsp-1 ) }
      
      while( gap > 0 ){
        poke(c.io.xin, BigInt( rng.nextInt(10) ))
        gap -= 1
        vp -= 1
        step(1)
      }

      poke(c.io.incr, true)
      poke(c.io.xin, BigInt( rng.nextInt(10) ))
      vp -= 1
      step(1)
      poke(c.io.incr, false)

      while( vp > 0 ){
        poke(c.io.xin, BigInt( rng.nextInt(10) ))
        vp -= 1
        step(1)
      }

      vp = p
      gap = n2 - nsp

      println( gap, vp )

      poke(c.io.load, true)
      if ( nsp == 0 ){ 
        poke(c.io.xin, BigInt( rng.nextInt(10) ))
        gap -= 1
        vp -= 1
      }
      step(1)
      poke(c.io.load, false)
      
    }

    var ii = 5 // initialtion interval
    var ifunc = ii + n/p - 2   // number of cycles before func should be asserted
    var rem = n
    
    var f3 = false // hadamard function started
    var ftim = 0 // cycles of hadamard function
    var cih = 0 // cycles of ih (this toggles every d cycles)
    while( rem>0 ){  
      step(1)
      rem -= 1

      if( rem == ifunc){
        poke(c.io.func, 0) //g mul func
        poke(c.io.ih, true)
      }

      if( rem == (ifunc - ii) ){
        poke(c.io.write, true)
      }

      if( rem == (ifunc - n/p) ){
        poke(c.io.func, 7) //stall
        poke(c.io.ih, false)
      }

      if( f3 ){
        ftim += 1
        cih += 1
        if( cih == d-1 ){
          cih = 0
          poke(c.io.ih, !(peek(c.io.ih)==1) )
        }
      }

      /*
      if( ftim == n ){
        f3 = false
      }*/

      if( rem == min(ifunc-n/p, ifunc-ii)-1 ){
        poke(c.io.func, 3)
        poke(c.io.ih, false)
        poke(c.io.hon, true)
        f3 = true
      }

      if( rem == 3 ){
        poke(c.io.incr, true)
      }

      if( rem == 2 ){
        poke(c.io.incr, false)
        poke(c.io.incr, false)
      }
    }
    poke(c.io.s1, false)

    println(ftim, cih)
    println(ftim, cih)
    println(ftim, cih)
    println(ftim, cih)


    var tI = (n/p)*d - ftim // total iterations left
    var iT = tI             // starting point

    /*
    Valid Output of PHBx here 
    -------------------------
    Move:
      - c.io.func = 0 (5 cycles before this point)
      - keep high for n/p cycles
      - stall for (5 - n/p) cycles *if required
      - #5 comes from 4 cycles delaying opcode (this is for
            the read/write to Mem) and 1 cycle for the ALU
    */

    // if not already asserted
    poke(c.io.write, true) //if not already asserted
    if( n != p){ //if n==p, func=3 and hon asserted next cycle (data not written back yet)
      poke(c.io.func, 3)  // hadamard
      poke(c.io.hon, true)
    }
    
    if( n==p ){
      tI +=1  // add an extra cycle (we must wait for data to be written to Mem)
    }

    while( tI > 0 ){
      step(1)
      tI -= 1
      cih +=1

      // rstG on for n/p cycles
      if( tI == iT-1 ){
        poke(c.io.rstG, true)
        if( n==p ){
          poke(c.io.func, 3)
          poke(c.io.hon, true)
        }
      }
      if( tI == (iT-1-n/p) ){
        poke(c.io.rstG, false)
      }

      // make sure ih toggles every d cycles
      if( (peek(c.io.ih)==1) ){
        poke(c.io.ih, false)
        cih = 0
      }

      if( cih == d-1 ){
        cih = 0
        poke(c.io.ih, !(peek(c.io.ih)==1) )
      }

      // turn off the write (*still parameterise this*)
      if( tI == iT - min(2, n/p) ){//iT-2 ){
        poke(c.io.write, false)
      }
    }
    if( n==p ){
      step(1) // only one output per neuron
    }
    poke(c.io.func, 7)
    poke(c.io.ih, false)
    poke(c.io.hon, false)

    // multiply S
    poke(c.io.func, 1)
    poke(c.io.ih, true)
    step( n/p )
    poke(c.io.ih, false)
    poke(c.io.func, 7)

    // cosine function
    poke(c.io.func, 4)
    poke(c.io.ih, true)
    step(2)
    poke(c.io.ih, false)
    poke(c.io.write, true)
    
    // multiply alpha ( and finish writing to kernel )
    poke(c.io.func, 2)
    poke(c.io.ih, true)
    step(2)
    poke(c.io.write, false) // finish writing
    poke(c.io.ih, false)
    
    // accumulate neurons in each PE
    poke(c.io.func, 5)
    poke(c.io.ih, true)
    step(2)
    poke(c.io.ih, false)

    // sum across PEs
    poke(c.io.func, 6)
    step(p) //step((p-1)*2)
    poke(c.io.func, 7)

    step(1)
    poke(c.io.func, 7)

      // 4. finish
    for(ix <- 0 until 10){
      step(1)
    }

  }

  // full working version, not flexible 
  def pe842(){

    for( ix <- 0 until 1){
      poke(c.io.xin, BigInt( rng.nextInt(10) ) )
      step(1)
    }
    poke(c.io.xin, BigInt( rng.nextInt(10) ) )
    poke(c.io.proc , true)
    step(1)

    poke(c.io.load, true)
    step(1)
    poke(c.io.load, false) 

    poke(c.io.s1, true)
    for( ix <- 0 until 1 ){
      step(1)
    }

    // for more loops of xin

    poke(c.io.incr, true) // 2 cycles earlier
    poke(c.io.xin, BigInt( rng.nextInt(10) ))
    step(1)
    poke(c.io.incr, false)

    for( ix <- 0 until 1){
      poke(c.io.xin, BigInt( rng.nextInt(10) ))
      step(1)
    }
    poke(c.io.load, true)
    //poke(c.io.incr, true)
    step(1)
    poke(c.io.load, false)
    //poke(c.io.incr, false)
    for( ix <- 0 until 1 ){
      step(1)
    }

    poke(c.io.incr, true)
    poke(c.io.xin, BigInt( rng.nextInt(10) ))
    step(1)
    poke(c.io.incr, false)

    for( ix <- 0 until 1){
      poke(c.io.xin, BigInt( rng.nextInt(10) ))
      step(1)
    }
    poke(c.io.load, true)
    
    // overlap with datapath 2 (must start 3 cycles early)
    poke(c.io.func, 0) // 2
    poke(c.io.ih, true) //2
    
    //poke(c.io.incr, true)
    step(1)
    poke(c.io.load, false)
    //poke(c.io.incr, false)
    for( ix <- 0 until 1 ){
      step(1)
    }
    poke(c.io.incr, true)
    
    poke(c.io.ih, false) 
    poke(c.io.func, 7) //stall for a few cycles
    
    step(1)
    poke(c.io.incr, false)
    poke(c.io.proc, false)

    step(2)
    poke(c.io.s1, false)
    poke(c.io.write, true)

    poke(c.io.func, 3)  // hadamard
    poke(c.io.hon, true)
    step(1)
    poke(c.io.ih, true)
    poke(c.io.rstG, true)
    step(1)
    poke(c.io.ih, false)
    poke(c.io.write, false)
    step(1)
    poke(c.io.ih, true)
    poke(c.io.rstG, false)
    step(1)
    poke(c.io.func, 7)
    poke(c.io.ih, false)
    poke(c.io.hon, false)

    // multiply S
    poke(c.io.func, 1)
    poke(c.io.ih, true)
    step(2)
    poke(c.io.ih, false)
    poke(c.io.func, 7)

    // cosine function
    poke(c.io.func, 4)
    poke(c.io.ih, true)
    step(2)
    poke(c.io.ih, false)
    poke(c.io.write, true)
    
    // multiply alpha ( and finish writing to kernel )
    poke(c.io.func, 2)
    poke(c.io.ih, true)
    step(2)
    poke(c.io.write, false) // finish writing
    poke(c.io.ih, false)
    
    // accumulate neurons in each PE
    poke(c.io.func, 5)
    poke(c.io.ih, true)
    step(2)
    poke(c.io.ih, false)

    // sum across PEs
    poke(c.io.func, 6)
    step(2)
    poke(c.io.func, 7)
    

    step(1)
    poke(c.io.func, 7)

      // 4. finish
    for(ix <- 0 until 10){
      step(1)
    }
  }


}

  
object pe42Tester{

  val bitWidth = 18
  val fracWidth = 10
  val n = 16
  val p = 8
  val d = 8


  def main(args: Array[String]): Unit = {
    println("Testing the PE for the fastfood array")
    
    chiselMainTest(Array("--genHarness", "--test", "--backend", "c", //"--wio", 
      "--compile", "--targetDir", ".emulator", "--vcd"), // .emulator is a hidden directory
      () => Module(new Array42( bitWidth, fracWidth, n, p, d ) ) ) {
        f => new pe42Sim( f )
      }
  }

} 


object pe42Verilog{

  val bitWidth = 18
  val fracWidth = 10
  val n = 4
  val p = 2
  val d = 8

  val rng = new Random( 33 )
  val weights = List(1,0,1,0,1,1,1,1)

  def main(args: Array[String]): Unit = {
    println("Generating verilog for the 4-4-4 array of PEs")
    chiselMain(Array("--backend", "v", "--targetDir", "verilog"), 
                () => Module( new Array42( bitWidth, fracWidth, n, p, d ) ) ) //PE42( 0, bitWidth, fracWidth, weights, n, p, d ) ) )
  }


}
