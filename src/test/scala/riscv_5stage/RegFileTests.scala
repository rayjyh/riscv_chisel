// See LICENSE for license details.

package riscv_5stage

import scala.util.Random
//import chiselTests.ChiselFlatSpec
import chisel3._
import chisel3.util._
import chisel3.testers._
import chiseltest._
import org.scalatest._

//import ALU._



class RegFileTests extends org.scalatest.FlatSpec with TestUtils{
  //implicit val p = (new MiniConfig).toInstance
  behavior of "RegFile"
  it should "" in {
    //assert(TesterDriver execute (() => new ALUTester(new ALU)))
    test(new RegFile(32)) {c =>
	for (i <- 0 until 32){
		c.io.rs1.poke(i)
		c.io.rs2.poke(i)
		c.io.RegWrite.poke(false.B)
		c.io.rs1_data.expect(0.U(32.W))	
		c.io.rs2_data.expect(0.U(32.W))
	}  
	c.clock.step(1)  
	val rnd = new Random()	
	val data  = Seq.fill(32)(rnd.nextInt()) map toBigInt
	for (i <- 0 until 32){
		c.io.RegWrite.poke(true.B)
		c.io.WriteReg.poke(i)
		c.io.WriteData.poke(data(i))
		c.clock.step(1)	
	}
	for (i <- 0 until 32){
		c.io.RegWrite.poke(false.B)
		c.io.rs1.poke(i)
		c.io.rs1_data.expect(data(i))
	}
    }

  }

}
