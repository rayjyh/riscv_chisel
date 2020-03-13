package riscv_5stage

import chisel3._
import chisel3.util._

class ControlIO(OpcodeWidth: Int, ALUSrcWidth: Int, ALUOpWidth: Int) extends Bundle {
	val opcode         	= Input(UInt(OpcodeWidth.W)) 
	val funct3			= Input(UInt(3.W))
	val funct7			= Input(UInt(7.W))
	val IF_flush_in		= Input(UInt(1.W))
	val Branch_outcome	= Input(UInt(1.W))

	val IF_flush		= Output(UInt(1.W))
	val ID_flush		= Output(UInt(1.W))
	val EX_flush		= Output(UInt(1.W))
	val MemRead			= Output(UInt(1.W))
	val MemWrite		= Output(UInt(1.W))
	val Jump			= Output(UInt(1.W))
	val Branch			= Output(UInt(1.W))
	val MemtoReg		= Output(UInt(1.W))
	val RegWrite		= Output(UInt(1.W))
	val ALUsrc			= Output(UInt(ALUSrcWidth.W))
	val alu_op    		= Output(UInt(ALUOpWidth.W))
}

class Control() extends Module with RVConfig with {
	val io = IO(new ControlIO)

	private val add_or_sub = Wire(UInt(ALUOpWidth.W))
	private val srl_or_sra = Wire(UInt(ALUOpWidth.W))

	add_or_sub := Mux(((io.opcode === RV_OP) && (io.funct7(5)) && (io.funct3 == RV_FUNCT3_ADD_SUB)), ALU_OP_SUB, ALU_OP_ADD)
	srl_or_sra := Mux(((io.opcode === RV_OP) && (io.funct7(5)) && (io.funct3 == RV_FUNCT3_SRA_SRL)), ALU_OP_SRA, ALU_OP_SRL)
	io.ID_flush := io.Branch_outcome
	io.IF_flush := io.Branch_outcome
	io.EX_flush := io.Branch_outcome

	when(IF_flush_in) {
		io.MemRead	:= 0.U
		io.MemWrite	:= 1.U
		io.Branch 	:= 1.U
		io.Jump 	:= 1.U
		io.MemtoReg	:= 1.U
		io.RegWrite	:= 1.U
		io.ALUsrc 	:= 0.U(ALU_SRC_WIDTH.W)
		io.alu_op 	:= 0.U(ALU_OP_WIDTH.W)
	}.elsewhen{
		switch(io.opcode){
			is(RV_BRANCH){
				io.MemRead	:= 0.U
				io.MemWrite := 0.U
				io.Branch 	:= 1.U
				io.Jump 	:= 0.U
				io.MemtoReg := 1.U
				io.RegWrite := 0.U
				io.ALUsrc 	:= SRC_B_RS2.U
				when(io.funct3 === RV_FUNCT3_BEQ){	io.alu_op := ALU_OP_SEQ}
				.elsewhen(io.funct3 === RV_FUNCT3_BNE){	io.alu_op := ALU_OP_SNE}
				.elsewhen(io.funct3 === RV_FUNCT3_BLT){	io.alu_op := ALU_OP_SLT}
				.elsewhen(io.funct3 === RV_FUNCT3_BGE){	io.alu_op := ALU_OP_SGE}
				.elsewhen(io.funct3 === RV_FUNCT3_BLTU){ io.alu_op := ALU_OP_SLTU}					
				.elsewhen(io.funct3 === RV_FUNCT3_BGEU){ io.alu_op := ALU_OP_SGEU}
				.otherwise{io.alu_op = ALU_OP_SEQ}
			}
			is(RV_LOAD){
				io.MemRead	:= 1.U
				io.MemWrite := 0.U
				io.Branch 	:= 0.U
				io.Jump 	:= 0.U
				io.MemtoReg := 1.U //1 for datamem read, 0 for alu result
				io.RegWrite := 1.U
				io.ALUsrc 	:= SRC_B_IMM_I.U
				io.alu_op 	:= ALU_OP_ADD.U
			}
			is(RV_STORE){
				io.MemRead 	:= 0.U
				io.MemWrite := 1.U
				io.Branch 	:= 0.U
				io.Jump 	:= 0.U
				io.MemtoReg := 1.U //1 for datamem read, 0 for alu result
				io.RegWrite := 0.U
				io.ALUsrc 	:= SRC_B_IMM_S.U
				io.alu_op 	:= ALU_OP_ADD.U
			}
			is(RV_IMM){
				io.MemRead 	:= 0.U
				io.MemWrite := 0.U
				io.Branch 	:= 0.U
				io.Jump 	:= 0.U
				io.MemtoReg := 0.U //1 for datamem read, 0 for alu result
				io.RegWrite := 1.U
				io.ALUsrc 	:= SRC_B_IMM_I.U
				when(io.funct3 === RV_FUNCT3_ADD_SUB){	io.alu_op := ALU_OP_ADD}
				.elsewhen(io.funct3 === RV_FUNCT3_SLT){	io.alu_op := ALU_OP_SLT}
				.elsewhen(io.funct3 === RV_FUNCT3_SLTU){io.alu_op := ALU_OP_SLTU}
				.elsewhen(io.funct3 === RV_FUNCT3_XOR){	io.alu_op := ALU_OP_XOR}						
				.elsewhen(io.funct3 === RV_FUNCT3_OR){	io.alu_op := ALU_OP_OR}
				.otherwise{io.alu_op := ALU_OP_ADD.U}
			}
			is(RV_OP){
				io.MemRead	= 0.U
				io.MemWrite = 0.U
				io.Branch 	= 0.U
				io.Jump 	= 0.U
				io.MemtoReg = 0.U
				io.RegWrite = 1.U
				io.ALUsrc 	= 0.U
				when(io.funct3 === RV_FUNCT3_ADD_SUB){		alu_op := add_or_sub}
				.elsewhen(io.funct3 === RV_FUNCT3_SLL){		alu_op := ALU_OP_SLL}
				.elsewhen(io.funct3 === RV_FUNCT3_SLT){		alu_op := ALU_OP_SLT}
				.elsewhen(io.funct3 === RV_FUNCT3_SLTU){		alu_op := ALU_OP_SLTU}
				.elsewhen(io.funct3 === RV_FUNCT3_XOR){		alu_op := ALU_OP_XOR}	
				.elsewhen(io.funct3 === RV_FUNCT3_SRA_SRL){	alu_op := srl_or_sra}						
				.elsewhen(io.funct3 === RV_FUNCT3_OR){		alu_op := ALU_OP_OR}
				.elsewhen(io.funct3 === RV_FUNCT3_AND){		alu_op := ALU_OP_AND}
				.otherwise{alu_op := ALU_OP_ADD}
			}
			is(RV_JAL){
				io.MemRead 	:= 0.U
				io.MemWrite := 0.U
				io.Branch 	:= 0.U
				io.Jump 	:= 1.U
				io.MemtoReg := 0.U //1 for datamem read, 0 for alu result
				io.RegWrite := 0.U
				io.ALUsrc 	:= 0.U
			}
			is(RV_SHAMT){
				io.MemRead 	:= 0.U
				io.MemWrite := 0.U
				io.Branch 	:= 0.U
				io.Jump 	:= 0.U
				io.MemtoReg := 0.U //1 for datamem read, 0 for alu result
				io.RegWrite := 0.U//changed***
				io.ALUsrc 	:= 0.U(ALU_SRC_WIDTH.W)
				io.alu_op 	:= 0.U(ALU_OP_WIDTH.W)
			}
		}
	}
}