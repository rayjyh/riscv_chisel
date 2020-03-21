

package riscv_5stage

import chisel3._
import chisel3.util._


object Const {
  val PC_START = 0x200
  val PC_EVEC  = 0x100
}



class DatapathIO(xlen: Int) extends Bundle {
  val icache = Flipped(new CacheIO(xlen))
  val dcache = Flipped(new CacheIO(xlen))
  val ctrl = Flipped(new ControlSignals(xlen))
}


class Datapath(xlen: Int) extends Module {
  val io      = IO(new DatapathIO(xlen))
  val regFile = Module(new RegFile(xlen)) 
  val alu     = Module(new ALU(xlen))//
  val immGen  = Module(new ImmGen(xlen))//
  val brCond  = Module(new BrCond(xlen))//


  import Control._

  /***** Fetch / Execute Registers *****/
  val fe_inst = RegInit(Instructions.NOP)
  val fe_pc   = Reg(UInt())

  /***** Execute / Write Back Registers *****/
  val ew_inst = RegInit(Instructions.NOP) 
  val ew_pc   = Reg(UInt())
  val ew_alu  = Reg(UInt())
  //val csr_in  = Reg(UInt())

  /****** Control signals *****/
  val st_type  = Reg(io.ctrl.st_type.cloneType)
  val ld_type  = Reg(io.ctrl.ld_type.cloneType)
  val wb_sel   = Reg(io.ctrl.wb_sel.cloneType)
  val wb_en    = Reg(Bool())
  val illegal  = Reg(Bool())
  val pc_check = Reg(Bool())
 
  /****** Fetch *****/
  val started = RegNext(reset.toBool)
  val stall = false.B//
  val pc   = RegInit(Const.PC_START.U(xlen.W) - 4.U(xlen.W))
  val npc  = Mux(stall, pc, //
             Mux(io.ctrl.pc_sel === PC_ALU || brCond.io.taken, alu.io.sum >> 1.U << 1.U,//
             Mux(io.ctrl.pc_sel === PC_0, pc, pc + 4.U)))//
  val inst = Mux(started || io.ctrl.inst_kill || brCond.io.taken, Instructions.NOP, io.icache.dout)//
  pc                      := npc 

  io.icache.addr := npc//
  io.icache.wen  := false.B//
  io.icache.din  := 0.U//
 
  // Pipelining
  when (!stall) {
    fe_pc   := pc
    fe_inst := inst
  }

  /****** Execute *****/
  // Decode
  io.ctrl.inst  := fe_inst

  // regFile read
  val rd_addr  = fe_inst(11, 7)
  val rs1_addr = fe_inst(19, 15)
  val rs2_addr = fe_inst(24, 20)
  regFile.io.raddr1 := rs1_addr
  regFile.io.raddr2 := rs2_addr

  // gen immdeates
  immGen.io.inst := fe_inst
  immGen.io.sel  := io.ctrl.imm_sel

  // bypass
  val wb_rd_addr = ew_inst(11, 7)
  val rs1hazard = wb_en && rs1_addr.orR && (rs1_addr === wb_rd_addr)
  val rs2hazard = wb_en && rs2_addr.orR && (rs2_addr === wb_rd_addr)
  val rs1 = Mux(wb_sel === WB_ALU && rs1hazard, ew_alu, regFile.io.rdata1) 
  val rs2 = Mux(wb_sel === WB_ALU && rs2hazard, ew_alu, regFile.io.rdata2)
 
  // ALU operations
  alu.io.A := Mux(io.ctrl.A_sel === A_RS1, rs1, fe_pc)
  alu.io.B := Mux(io.ctrl.B_sel === B_RS2, rs2, immGen.io.out)
  alu.io.alu_op := io.ctrl.alu_op

  // Branch condition calc
  brCond.io.rs1 := rs1 
  brCond.io.rs2 := rs2
  brCond.io.br_type := io.ctrl.br_type

  // D$ access
  io.dcache.addr := Mux(stall, ew_alu, alu.io.sum) >> 2.U << 2.U//
  io.dcache.wen  := !stall && io.ctrl.st_type.orR//
  io.dcache.din  := rs2//
  
  // Pipelining
  when(reset.toBool || !stall) {
    st_type   := 0.U
    ld_type   := 0.U
    wb_en     := false.B
    illegal   := false.B
    pc_check  := false.B
  }.elsewhen(!stall) {
    ew_pc     := fe_pc
    ew_inst   := fe_inst
    ew_alu    := alu.io.out
    st_type   := io.ctrl.st_type
    ld_type   := io.ctrl.ld_type
    wb_sel    := io.ctrl.wb_sel
    wb_en     := io.ctrl.wb_en
    illegal   := io.ctrl.illegal
    pc_check  := io.ctrl.pc_sel === PC_ALU
  }

  // Load
  val load = io.dcache.dout//

  // Regfile Write
  val regWrite = MuxLookup(wb_sel, ew_alu.zext, Seq(//
    WB_MEM -> load,//
    WB_PC4 -> (ew_pc + 4.U).zext)).asUInt//

  regFile.io.wen   := wb_en && !stall//
  regFile.io.waddr := wb_rd_addr
  regFile.io.wdata := regWrite


}
