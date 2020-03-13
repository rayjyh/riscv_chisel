package riscv_5stage

import chisel3._
import chisel3.util._

class RegFileIO(RegAddrWidth: Int, DataWidth: Int)  extends Bundle {
  val rs1       = Input(UInt(RegAddrWidth.W))
  val rs2       = Input(UInt(RegAddrWidth.W))
  val rs1_data  = Output(UInt(DataWidth.W))
  val rs2_data  = Output(UInt(DataWidth.W))
  val RegWrite  = Input(Bool())
  val WriteReg  = Input(UInt(RegAddrWidth.W))
  val WriteData = Input(UInt(DataWidth.W))
}

class RegFile() extends Module with CoreParams {
  val io = IO(new RegFileIO)
  val regs = Mem(32, UInt(DataWidth.W))
  io.rs1_data := Mux(io.rs1.orR, regs(io.rs1), 0.U)
  io.rs2_data := Mux(io.rs2.orR, regs(io.rs2), 0.U)
  when(io.RegWrite & io.RegWrite.orR) {
    regs(io.WriteReg) := io.WriteData
  }
}