// See LICENSE for license details.

package riscv_5stage

import scala.util.Random
//import chiselTests.ChiselFlatSpec
import chisel3._
import chisel3.util._
import chisel3.testers._

import ALU._

class ParameterizedALUTester(alu: => ParameterizedALU) extends BasicTester with TestUtils {
  val dut = Module(alu)
  //val ctrl = Module(new Control)
  val xlen = 32
  
  val rnd = new Random()
  val (cntr, done) = Counter(true.B, alu_op.size)
  val rs1  = Seq.fill(alu_op.size)(rnd.nextInt()) map toBigInt
  val rs2  = Seq.fill(alu_op.size)(rnd.nextInt()) map toBigInt
  val sum  = VecInit((rs1 zip rs2) map { case (a, b) => toBigInt(a.toInt + b.toInt).U(xlen.W) })
  val diff = VecInit((rs1 zip rs2) map { case (a, b) => toBigInt(a.toInt - b.toInt).U(xlen.W) })
  val and  = VecInit((rs1 zip rs2) map { case (a, b) => (a & b).U(xlen.W) })
  val or   = VecInit((rs1 zip rs2) map { case (a, b) => (a | b).U(xlen.W) })
  val xor  = VecInit((rs1 zip rs2) map { case (a, b) => (a ^ b).U(xlen.W) })
  val slt  = VecInit((rs1 zip rs2) map { case (a, b) => (if (a.toInt < b.toInt) 1 else 0).U(xlen.W) })
  val sltu = VecInit((rs1 zip rs2) map { case (a, b) => (if (a < b) 1 else 0).U(xlen.W) })
  val sll  = VecInit((rs1 zip rs2) map { case (a, b) => toBigInt(a.toInt << (b.toInt & 0x1f)).U(xlen.W) })
  val srl  = VecInit((rs1 zip rs2) map { case (a, b) => toBigInt(a.toInt >>> (b.toInt & 0x1f)).U(xlen.W) })
  val sra  = VecInit((rs1 zip rs2) map { case (a, b) => toBigInt(a.toInt >> (b.toInt & 0x1f)).U(xlen.W) })
  //val seq  = VecInit((rs1 zip rs2) map { case (a, b) => (if (toBigInt(a) === toBigInt(b)) 1 else 0).U(xlen.W) })
  //val sne  = VecInit((rs1 zip rs2) map { case (a, b) => (if (a =/= b) 1 else 0).U(xlen.W) })
  val sge  = VecInit((rs1 zip rs2) map { case (a, b) => (if (a.toInt >= b.toInt) 1 else 0).U(xlen.W) })
  val sgeu  = VecInit((rs1 zip rs2) map { case (a, b) => (if (a >= b) 1 else 0).U(xlen.W) })
  val out = Mux(dut.io.op === ALU_ADD,  sum(cntr),
             Mux(dut.io.op === ALU_SLL,  sll(cntr),
             Mux(dut.io.op === ALU_XOR,  xor(cntr),
             Mux(dut.io.op === ALU_OR,   or(cntr), 
             Mux(dut.io.op === ALU_AND,  and(cntr),
             Mux(dut.io.op === ALU_SRL,  srl(cntr),
             //Mux(dut.io.op === ALU_SEQ,  seq(cntr),//
             //Mux(dut.io.op === ALU_SNE,  sne(cntr),//
             Mux(dut.io.op === ALU_SUB,  diff(cntr),
             Mux(dut.io.op === ALU_SRA,  sra(cntr),
             Mux(dut.io.op === ALU_SLT,  slt(cntr),
             Mux(dut.io.op === ALU_SGE,  sge(cntr),
             Mux(dut.io.op === ALU_SLTU, sltu(cntr),sgeu(cntr))))))))))))

  dut.io.op := VecInit(alu_op)(cntr)
  dut.io.in1 := VecInit(rs1 map (_.U))(cntr)
  dut.io.in2 := VecInit(rs2 map (_.U))(cntr)

  when(done) { stop(); stop() } // from VendingMachine example...
  assert(dut.io.out === out)
  printf("Counter: %d, OP: 0x%x, IN1: 0x%x, IN2: 0x%x, OUT: 0x%x ?= 0x%x\n",
         cntr, dut.io.op, dut.io.in1, dut.io.in2, dut.io.out, out) 
}

class ALUTests extends org.scalatest.FlatSpec {
  //implicit val p = (new MiniConfig).toInstance
  val xlen = 32
  "ALU" should "pass" in {
    //assert(TesterDriver execute (() => new ALUTester(new ALU)))
    assert(
      TesterDriver execute (() => new ParameterizedALUTester(new ALU(xlen)))
    )
  }

}
