package riscv_5stage

import chisel3._
import chisel3.util._

class ALUSrcBMuxIO(ALUSrcWidth: Int, DataWidth: Int) extends Bundle {
	val ALUsrc 		= Input(UInt(ALUSrcWidth.W))
	val imm_I		= Input(UInt(DataWidth.W))
	val imm_S		= Input(UInt(DataWidth.W))
	val shamt_ext	= Input(UInt(DataWidth.W))
	val rs2_data	= Input(UInt(DataWidth.W))
	val alu_src_b	= Output(UInt(DataWidth.W))
}

class ALUSrcBMux() extends Module{
	val io = IO(new ALUSrcBMuxIO)

	when(io.ALUsrc === SRC_B_RS2){ io.alu_src_b := io.rs2_data}
    when(io.ALUsrc === SRC_B_IMM_I{io.alu_src_b := io.imm_I}
    when(io.ALUsrc === SRC_B_IMM_S{io.alu_src_b := io.imm_S}
    when(io.ALUsrc === SRC_B_SHAMT{io.alu_src_b := io.shamt_ext}
    .otherwise{io.alu_src_b := 0.U}
}