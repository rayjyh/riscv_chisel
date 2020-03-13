package riscv_5stage

import chisel3._
import chisel3.util._

class DatapathIO() extends Bundle{

}

class Datapath(ALUOpWidth: Int, ALUSrcWidth: Int) extends Module{
	val io = IO(new DatapathIO)

	val alu  	= Module(new ALU)
  	val bmux 	= Module(new ALUSrcBMux)
  	val icache 	= Module(new Cache)
  	val dcache  = Module(new Cache)
  	val ctrl  	= Module(new Control)
  	val regFile = Module(new RegFile)

  	//***IF/ID Registers***//
  	val fd_pc 	 = RegInit(0.U(PC_LEN.W))
  	val fd_flush = RegInit(0.U(1.W))
  	val fd_inst  = RegInit(0.U(32.W))

  	//***ID/EX Registers***//
  	val de_MemtoReg  = RegInit(0.U(1.W))
	val de_RegWrite  = RegInit(0.U(1.W))
	val de_MemRead  = RegInit(0.U(1.W))
	val de_MemWrite  = RegInit(0.U(1.W))
	val de_Branch  = RegInit(0.U(1.W))
	val de_Jump  = RegInit(0.U(1.W))
	val de_alu_op  = RegInit(0.U(ALUOpWidth.W))
	val de_ALUsrc  = RegInit(0.U(ALUSrcWidth.W))
	val de_PC  = RegInit(0.U(PC_LEN.W))
	val de_rs1_data  = RegInit(0.U(DataWidth.W))
	val de_rs2_data  = RegInit(0.U(DataWidth.W))
	val de_rs1  = RegInit(0.U(RegAddrWidth.W))
	val de_rs2  = RegInit(0.U(RegAddrWidth.W))
	val de_rd   = RegInit(0.U(RegAddrWidth.W))
	val de_imm_funct7  = RegInit(0.U(7.W))
	val de_funct3  = RegInit(0.U(3.W))

	//***EX/ME Registers***//
	val em_MemtoReg   = RegInit(0.U(1.W))
	val em_RegWrite   = RegInit(0.U(1.W))
	val em_MemRead   = RegInit(0.U(1.W))
	val em_MemWrite   = RegInit(0.U(1.W))
	val em_Branch   = RegInit(0.U(1.W))
	val em_Jump   = RegInit(0.U(1.W))
	val em_PC   = RegInit(0.U(PC_LEN.W))
	val em_ALU_result = RegInit(0.U(DataWidth.W))
	val em_STORE_data = RegInit(0.U(DataWidth.W))
	val em_Write_Reg = RegInit(0.U(RegAddrWidth.W))

	//***ME/WB Registers***//
	val mw_MemtoReg = RegInit(0.U(1.W))
	val mw_RegWrite = RegInit(0.U(1.W))
 	val mw_Reg_data = RegInit(0.U(DataWidth.W))
 	val mw_Write_Reg = RegInit(0.U(RegAddrWidth.W))

 	//***IF***//
 	val pc = RegInit(0.U(PC_LEN.W))
 	val pc_next = Mux(Branch_outcome, em_PC, pc+1.U)

 	// Pipelining
 	pc := pc_next
 	icache.io.addr := pc
 	icache.io.we := 0.U
 	icache.io.din := 0.U
 	fd_inst := icache.io.dout

 	//***ID***//
 	private val opcode	  	  = Wire(UInt(7.W)) fd_inst[6:0]
	private val rd_ID	  	  = Wire(UInt(5.W)) fd_inst[11:7]
	private val funct3_ID 	  = Wire(UInt(3.W)) fd_inst[14:12]
	private val rs1_ID	  	  = Wire(UInt(5.W)) fd_inst[19:15]
	private val rs2_ID	  	  = Wire(UInt(5.W)) fd_inst[24:20]
	private val imm_funct7_ID = Wire(UInt(7.W)) fd_inst[31:25]

}