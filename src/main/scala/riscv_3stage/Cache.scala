
package riscv_3stage

import chisel3._
//import freechips.rocketchip.config.Parameters

class CacheIO(val xlen: Int = 32) extends Bundle {
  val addr = Input(UInt(xlen.W))
  val dout = Output(UInt(xlen.W))
  val wen  = Input(Bool())
  val din  = Input(UInt(xlen.W))
}

class Cache(xlen: Int = 32) extends Module{
  val io = IO(new CacheIO(xlen))
  val cache = Mem(32, UInt(xlen.W))

  when(io.wen) {
    cache.write(io.addr, io.din)
    io.dout := DontCare
  }
  
  io.dout := cache.read(io.addr)
}
