

package riscv_5stage

import chisel3._
import chisel3.util._
import chisel3.testers._


class DatapathTester(datapath: => Datapath) extends BasicTester with TestUtils {
  val xlen = 32
  val dut = Module(datapath)
  val ctrl = Module(new Control(xlen))

  dut.io.ctrl <> ctrl.io

  override val insts = bubblesortTest

  val sImemInit :: sDmemInit :: sRun :: Nil = Enum(2)
  val state = RegInit(sInit)
  val (cntr, done) = Counter(state === sInit, insts.size)
  val timeout = RegInit(0.U(32.W))

  val imem = Mem(1 << 20, UInt(xlen.W))//
  val dmem = Mem(1 << 20, UInt(xlen.W))//
  val iaddr = dut.io.icache.addr
  val daddr = dut.io.dcache.addr
  val write = dut.io.dcache.din
  dut.reset := state === sInit

  dut.io.icache.dout := RegNext(imem(iaddr))
  dut.io.dcache.dout := RegNext(dmem(daddr))

  switch(state) {
    is(sImemInit) {
      printf("Imem Init Begins!\n")
      imem(Const.PC_START.U + cntr) := VecInit(insts)(cntr)
      when(done) { state := sDmemInit }
    }
    is(sDmemInit) {
      printf("Dmem Init Begins!\n")
      for (i <- 0 util 5) {dmem(i) := 5.U - i}
      state := sRun
    }
    is(sRun) {
      when(!dut.io.icache.wen) {
        printf(s"INST[%x] => %x, iaddr: %x\n", dut.io.icache.addr, imem(iaddr), iaddr)
      }
      when(true.B) {
        when(dut.io.dcache.wen) {
          dmem(daddr) := write
          printf("MEM[%x] <= %x\n", dut.io.dcache.addr, write)
        }.otherwise {
          printf("MEM[%x] => %x\n", dut.io.dcache.addr, dmem(daddr))
        }
      }
      timeout := timeout + 1.U
      assert(timeout < 100.U)
      when(iaddr === 30.U) {
        assert(dmem(12) === 10.U)
        printf("Datapath Test Finishes!\n")
        printf("dem(12) = %x\n", dmem(12))
        stop(); stop()
      }
    }
  }
}

class DatapathTests extends org.scalatest.FlatSpec {
  val xlen = 32
  Seq(BypassTest) foreach { test =>
    "Datapath" should s"pass $test" in {
      assert(TesterDriver execute (() => new DatapathTester(new Datapath(xlen))))
    }
  }
}
