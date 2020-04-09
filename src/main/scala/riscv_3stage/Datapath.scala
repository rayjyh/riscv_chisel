// See LICENSE for license details.

package riscv_3stage

import chisel3._
import chisel3.util._
//import freechips.rocketchip.config.Parameters

object Const {
  val PC_START = 0x200
  val PC_EVEC  = 0x100
}

//class DatapathIO(implicit p: Parameters) extends CoreBundle()(p) {
//  val host = new HostIO
//  val icache = Flipped(new CacheIO)
//  val dcache = Flipped(new CacheIO)
//  val ctrl = Flipped(new ControlSignals)
//}

class DatapathDebugIO(xlen: Int = 32) extends Bundle{
  val alu_out = Output(UInt(xlen.W))
  val ew_alu = Output(UInt(xlen.W))
  val regFile_wen = Output(Bool())
  val regFile_waddr = Output(UInt(5.W))
  val regFile_wdata = Output(UInt(xlen.W))
}

class DatapathIO(xlen: Int = 32) extends Bundle{
  val icache = Flipped(new CacheIO(xlen))
  val dcache = Flipped(new CacheIO(xlen))
  val ctrl = Flipped(new ControlSignals(xlen))
  val debug = new DatapathDebugIO(xlen)
}

//class Datapath(implicit val p: Parameters) extends Module with CoreParams {
//class Datapath(xlen: Int = 32, debug: Boolean) extends Module{
class Datapath(debug: Boolean)(implicit xlen: Int) extends Module {
  val io: DatapathIO = IO(new DatapathIO(xlen))
  //val csr     = Module(new CSR)
  val regFile = Module(new RegFile)//(xlen))
  val alu     = Module(new ALU)//(xlen))//
  val immGen  = Module(new ImmGen)//(xlen))//
  val brCond  = Module(new BrCond)//(xlen))//
  //val alu     = p(BuildALU)(p)
  //val immGen  = p(BuildImmGen)(p)
  //val brCond  = p(BuildBrCond)(p)

  import Control._

  /***** Fetch / Execute Registers *****/
  val fe_inst = RegInit(Instructions.NOP)
  val fe_pc: UInt = Reg(UInt())

  /***** Execute / Write Back Registers *****/
  val ew_inst = RegInit(Instructions.NOP) 
  val ew_pc: UInt = Reg(UInt())
  val ew_alu: UInt = Reg(UInt())
  //val csr_in  = Reg(UInt())

  /****** Control signals *****/
  val st_type: Datapath.this.io.ctrl.st_type.type = Reg(io.ctrl.st_type.cloneType)
  val ld_type: Datapath.this.io.ctrl.ld_type.type = Reg(io.ctrl.ld_type.cloneType)
  val wb_sel: Datapath.this.io.ctrl.wb_sel.type = Reg(io.ctrl.wb_sel.cloneType)
  val wb_en: Bool = Reg(Bool())
  //val csr_cmd  = Reg(io.ctrl.csr_cmd.cloneType)
  val illegal: Bool = Reg(Bool())
  val pc_check: Bool = Reg(Bool())
 
  /****** Fetch *****/
  val started = RegNext(reset.toBool)
  //val stall = !io.icache.resp.valid || !io.dcache.resp.valid
  val stall: Bool = false.B//
  //val pc   = RegInit(Const.PC_START.U(xlen.W) - 4.U(xlen.W))
  //val npc  = Mux(stall, pc, Mux(csr.io.expt, csr.io.evec,
  //           Mux(io.ctrl.pc_sel === PC_EPC,  csr.io.epc,
  //           Mux(io.ctrl.pc_sel === PC_ALU || brCond.io.taken, alu.io.sum >> 1.U << 1.U,
  //           Mux(io.ctrl.pc_sel === PC_0, pc, pc + 4.U)))))
  val pc   = RegInit(Const.PC_START.U(xlen.W) - 1.U(xlen.W))
  val npc  = Mux(stall, pc, //
             Mux(io.ctrl.pc_sel === PC_ALU || brCond.io.taken, alu.io.sum >> 1.U << 1.U,//
             Mux(io.ctrl.pc_sel === PC_0, pc, pc + 1.U)))//
  //val inst = Mux(started || io.ctrl.inst_kill || brCond.io.taken || csr.io.expt, Instructions.NOP, io.icache.resp.bits.data)
  val inst = Mux(started || io.ctrl.inst_kill || brCond.io.taken, Instructions.NOP, io.icache.dout)//
  pc                      := npc 
  //io.icache.req.bits.addr := npc
  //io.icache.req.bits.data := 0.U
  //io.icache.req.bits.mask := 0.U
  //io.icache.req.valid     := !stall
  //io.icache.abort         := false.B
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
  val rd_addr: UInt = fe_inst(11, 7)
  val rs1_addr: UInt = fe_inst(19, 15)
  val rs2_addr: UInt = fe_inst(24, 20)
  regFile.io.raddr1 := rs1_addr
  regFile.io.raddr2 := rs2_addr

  // gen immdeates
  immGen.io.inst := fe_inst
  immGen.io.sel  := io.ctrl.imm_sel

  // bypass
  val wb_rd_addr: UInt = ew_inst(11, 7)
  val rs1hazard: Bool = wb_en && rs1_addr.orR && (rs1_addr === wb_rd_addr)
  val rs2hazard: Bool = wb_en && rs2_addr.orR && (rs2_addr === wb_rd_addr)
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
  //val daddr   = Mux(stall, ew_alu, alu.io.sum) >> 2.U << 2.U
  //val woffset = alu.io.sum(1) << 4.U | alu.io.sum(0) << 3.U
  //io.dcache.req.valid     := !stall && (io.ctrl.st_type.orR || io.ctrl.ld_type.orR)
  //io.dcache.req.bits.addr := daddr 
  //io.dcache.req.bits.data := rs2 << woffset
  //io.dcache.req.bits.mask := MuxLookup(Mux(stall, st_type, io.ctrl.st_type), 
  //            "b0000".U, Seq(
  //  ST_SW ->  "b1111".U,
  //  ST_SH -> ("b11".U << alu.io.sum(1,0)),
  //  ST_SB -> ("b1".U  << alu.io.sum(1,0))))
  
  io.dcache.addr := Mux(stall, ew_alu, alu.io.sum) //
  io.dcache.wen  := !stall && io.ctrl.st_type.orR//
  io.dcache.din  := rs2//
  
  // Pipelining
  //when(reset.toBool || !stall && csr.io.expt) {
  //  st_type   := 0.U
  //  ld_type   := 0.U
  //  wb_en     := false.B
  //  csr_cmd   := 0.U
  //  illegal   := false.B
  //  pc_check  := false.B
  //}.elsewhen(!stall && !csr.io.expt) {
  //  ew_pc     := fe_pc
  //  ew_inst   := fe_inst
  //  ew_alu    := alu.io.out
  //  csr_in    := Mux(io.ctrl.imm_sel === IMM_Z, immGen.io.out, rs1)
  //  st_type   := io.ctrl.st_type
  //  ld_type   := io.ctrl.ld_type
  //  wb_sel    := io.ctrl.wb_sel
  //  wb_en     := io.ctrl.wb_en
  //  csr_cmd   := io.ctrl.csr_cmd
  //  illegal   := io.ctrl.illegal
  //  pc_check  := io.ctrl.pc_sel === PC_ALU
  //}
  when(reset.toBool) {
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
  //val loffset = ew_alu(1) << 4.U | ew_alu(0) << 3.U
  //val lshift  = io.dcache.resp.bits.data >> loffset
  //val load    = MuxLookup(ld_type, io.dcache.resp.bits.data.zext, Seq(
  //  LD_LH  -> lshift(15, 0).asSInt, LD_LB  -> lshift(7, 0).asSInt,
  //  LD_LHU -> lshift(15, 0).zext,   LD_LBU -> lshift(7, 0).zext) )
  val load: UInt = io.dcache.dout//

    
  // CSR access
  //csr.io.stall    := stall
  //csr.io.in       := csr_in
  //csr.io.cmd      := csr_cmd
  //csr.io.inst     := ew_inst
  //csr.io.pc       := ew_pc
  //csr.io.addr     := ew_alu
  //csr.io.illegal  := illegal
  //csr.io.pc_check := pc_check
  //csr.io.ld_type  := ld_type
  //csr.io.st_type  := st_type
  //io.host <> csr.io.host 

  // Regfile Write
  //val regWrite = MuxLookup(wb_sel, ew_alu.zext, Seq(
  //  WB_MEM -> load,
  //  WB_PC4 -> (ew_pc + 4.U).zext,
  //  WB_CSR -> csr.io.out.zext) ).asUInt 
  val regWrite: UInt = MuxLookup(wb_sel, ew_alu.zext.asUInt, Seq(//
    WB_MEM -> load.asUInt,//
    WB_PC4 -> (ew_pc + 1.U).zext.asUInt)).asUInt//

  //regFile.io.wen   := wb_en && !stall && !csr.io.expt 
  regFile.io.wen   := wb_en && !stall//
  regFile.io.waddr := wb_rd_addr
  regFile.io.wdata := regWrite

  // Abort store when there's an excpetion
  //io.dcache.abort := csr.io.expt

  //if (p(Trace)) {
  //  printf("PC: %x, INST: %x, REG[%d] <- %x\n", ew_pc, ew_inst,
  //    Mux(regFile.io.wen, wb_rd_addr, 0.U),
  //    Mux(regFile.io.wen, regFile.io.wdata, 0.U))
  //}
  if(debug){
    io.debug.alu_out := alu.io.out
    io.debug.ew_alu := ew_alu
    io.debug.regFile_wen := regFile.io.wen
    io.debug.regFile_waddr := regFile.io.waddr
    io.debug.regFile_wdata := regFile.io.wdata
  }
  else{
    io.debug <> DontCare
  }
}
