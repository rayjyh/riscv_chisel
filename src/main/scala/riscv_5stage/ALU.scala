package riscv_5stage

import chisel3._
import chisel3.util._

object ALU {
	val ALU_ADD   =  0.U(4.W)     
	val ALU_SLL   =  1.U(4.W)
	val ALU_XOR   =  4.U(4.W)
	val ALU_OR    =  6.U(4.W)
	val ALU_AND   =  7.U(4.W)
	val ALU_SRL   =  5.U(4.W)
	val ALU_SEQ   =  8.U(4.W)
	val ALU_SNE   =	 9.U(4.W)
	val ALU_SUB   =  10.U(4.W)
	val ALU_SRA   =  11.U(4.W)
	val ALU_SLT   =  12.U(4.W)
	val ALU_SGE   =  13.U(4.W)
	val ALU_SLTU  =	 14.U(4.W)				
	val ALU_SGEU  =  15.U(4.W)
}

class ALUIO(DataWidth: Int) extends Bundle{
	val op = Input(UInt(4.W))
  	val in1 = Input(UInt(DataWidth.W))
  	val in2 = Input(UInt(DataWidth.W))
  	val out = Output(UInt(DataWidth.W))
}

import ALU._

abstract class ParameterizedALU(DataWidth: Int) extends Module{
  val io = IO(new ALUIO(DataWidth))
}

class ALU(DataWidth: Int) extends ParameterizedALU(DataWidth){
	val shamt = io.in2(4,0).asUInt

	io.out := MuxLookup(io.op, 0.U(DataWidth.W), Seq(
      	ALU_ADD  -> (io.in1 + io.in2),
      	ALU_SLL  -> (io.in1 << shamt),
      	ALU_XOR  -> (io.in1 ^ io.in2),
      	ALU_OR   -> (io.in1 | io.in2),
      	ALU_AND  -> (io.in1 & io.in2),
      	ALU_SRL  -> (io.in1 >> shamt),
      	ALU_SEQ  -> (io.in1 === io.in2),
      	ALU_SNE  -> (io.in1 =/= io.in2),
      	ALU_SUB  -> (io.in1 - io.in2),
      	ALU_SRA  -> (io.in1.asSInt >> shamt).asUInt,
      	ALU_SLT  -> (io.in1.asSInt < io.in2.asSInt),
      	ALU_SGE  -> (io.in1.asSInt >= io.in2.asSInt),
      	ALU_SLTU -> (io.in1 < io.in2),
      	ALU_SGEU -> (io.in1 >= io.in2)))
}


