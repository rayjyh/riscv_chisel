// See LICENSE for license details.

package riscv_3stage

import chisel3._
import chisel3.util._
//import freechips.rocketchip.config.Parameters

object ALU {
  val ALU_ADD: UInt = 0.U(4.W)
  val ALU_SUB: UInt = 1.U(4.W)
  val ALU_AND: UInt = 2.U(4.W)
  val ALU_OR: UInt = 3.U(4.W)
  val ALU_XOR: UInt = 4.U(4.W)
  val ALU_SLT: UInt = 5.U(4.W)
  val ALU_SLL: UInt = 6.U(4.W)
  val ALU_SLTU: UInt = 7.U(4.W)
  val ALU_SRL: UInt = 8.U(4.W)
  val ALU_SRA: UInt = 9.U(4.W)
  val ALU_COPY_A: UInt = 10.U(4.W)
  val ALU_COPY_B: UInt = 11.U(4.W)
  val ALU_XXX: UInt = 15.U(4.W)
}

//class ALUIo(implicit p: Parameters) extends CoreBundle()(p) {
class ALUIO(xlen: Int) extends Bundle {
  val A = Input(UInt(xlen.W))
  val B = Input(UInt(xlen.W))
  val alu_op = Input(UInt(4.W))
  val out = Output(UInt(xlen.W))
  val sum = Output(UInt(xlen.W))
}

import riscv_3stage.ALU._

//abstract class ALU(implicit val p: Parameters) extends Module with CoreParams {
//  val io = IO(new ALUIo)
//}

//class ALUSimple(implicit val p: Parameters) extends ALU()(p) {
//class ALU(xlen: Int) extends Module with Config {
class ALU(implicit xlen: Int) extends Module {
  val io: ALUIO = IO(new ALUIO(xlen))
  val shamt: UInt = io.B(4,0).asUInt

  io.out := MuxLookup(io.alu_op, io.B, Seq(
      ALU_ADD  -> (io.A + io.B),
      ALU_SUB  -> (io.A - io.B),
      ALU_SRA  -> (io.A.asSInt >> shamt).asUInt,
      ALU_SRL  -> (io.A >> shamt),
      ALU_SLL  -> (io.A << shamt),
      ALU_SLT  -> (io.A.asSInt < io.B.asSInt),
      ALU_SLTU -> (io.A < io.B),
      ALU_AND  -> (io.A & io.B),
      ALU_OR   -> (io.A | io.B),
      ALU_XOR  -> (io.A ^ io.B),
      ALU_COPY_A -> io.A))

  io.sum := io.A + Mux(io.alu_op(0), -io.B, io.B)
}


