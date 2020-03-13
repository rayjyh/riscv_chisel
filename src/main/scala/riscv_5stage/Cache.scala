package riscv_5stage

import chisel3._
import chisel3.util._

class CacheIO(CacheAddrWidth: Int, DataWidth: Int)  extends Bundle {
  val addr  = Input(UInt(CacheAddrWidth.W))
  val dout  = Output(UInt(DataWidth.W))
  val we    = Input(Bool())
  val din   = Input(UInt(DataWidth.W))
}

class Cache(CacheSize: Int, DataWidth: Int) extends Module{
  val io = IO(new RegFileIO)
  val cache: Vec[UInt] = RegInit(VecInit(Seq.fill(CacheSize)(0.U(DataWidth.W))))
  //private val dataWire: UInt = Wire(UInt(DataWidth.W))
  when(io.we){cache(io.addr) := io.din}
  io.out := cache(io.addr)
}