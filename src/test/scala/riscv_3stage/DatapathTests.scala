// See LICENSE for license details.

package riscv_3stage

import chisel3._
import chisel3.testers._
import chisel3.util._

//class DatapathTester(datapath: => Datapath,
//                     testType: DatapathTest)
//                    (implicit p: freechips.rocketchip.config.Parameters) extends BasicTester with TestUtils {
//  val dut = Module(datapath)
//  val ctrl = Module(new Control)
//  val xlen = p(XLEN)

class DatapathTester(datapath: => Datapath)(implicit xlen: Int) extends BasicTester with TestUtils {
  //val xlen = 32
  val dut = Module(datapath)
  val ctrl = Module(new Control)//(xlen))

  dut.io.ctrl <> ctrl.io
  //dut.io.host.fromhost.bits := DontCare
  //dut.io.host.fromhost.valid := false.B

  //override val insts = tests(testType)
  override val insts = bubbleSort//bypassTest

  //val sInit :: sRun :: Nil = Enum(2)
  //val state = RegInit(sInit)
  //val (cntr, done) = Counter(state === sInit, insts.size)
  val sImemInit :: sDmemInit :: sRun :: Nil = Enum(3)
  val state = RegInit(sImemInit)
  val (cntr, done) = Counter(state === sImemInit, insts.size)
  val timeout = RegInit(0.U(32.W))
  //val mem = Mem(1 << 20, UInt(xlen.W))
  val imem = Mem(30, UInt(xlen.W))//
  val dmem = Mem(20, UInt(xlen.W))//
  //val iaddr = dut.io.icache.req.bits.addr / (xlen / 8).U
  //val daddr = dut.io.dcache.req.bits.addr / (xlen / 8).U
  val iaddr = dut.io.icache.addr
  val daddr = dut.io.dcache.addr
  //val write = ((0 until (xlen / 8)) foldLeft 0.U){ (data, i) => data |
  //  (Mux(dut.io.dcache.req.bits.mask(i), dut.io.dcache.req.bits.data, mem(daddr)) & (BigInt(0xff) << (8 * i)).U)
  //}
  val write = dut.io.dcache.din
  //dut.reset := state === sInit
  dut.reset := state === sImemInit
  //dut.io.icache.resp.bits.data := RegNext(mem(iaddr))
  //dut.io.icache.resp.valid := state === sRun
  //dut.io.dcache.resp.bits.data := RegNext(mem(daddr))
  //dut.io.dcache.resp.valid := state === sRun
  dut.io.icache.dout := RegNext(imem(iaddr))
  dut.io.dcache.dout := RegNext(dmem(daddr))
  val alu_out = dut.alu.io.out
  val alu_A = dut.alu.io.A
  val alu_B = dut.alu.io.B

  switch(state) {
    //is(sInit) {
    //  //(0 until Const.PC_START by 4) foreach { addr =>
    //  //  mem((addr / 4).U) := (if (addr == Const.PC_EVEC + (3 << 6)) fin else nop)
    //  //}
    //  //mem((Const.PC_START / (xlen / 8)).U + cntr) := VecInit(insts)(cntr)
    //  //when(done) { state := sRun }
    //  println("#####################")
    //  println("Datapath Init!")
    //  imem(Const.PC_START.U + cntr) := VecInit(insts)(cntr)
    //  printf(s"INST[%x] => %x\n", Const.PC_START.U + cntr - 3.U, imem(Const.PC_START.U + cntr - 3.U))
    //  when(done) { state := sRun }
    //}
    is(sImemInit) {
      printf("Imem Init Begins!\n")
      imem(Const.PC_START.U + cntr) := VecInit(insts)(cntr)
      when(done) { state := sDmemInit }
    }
    is(sDmemInit) {
      printf("Dmem Init Begins!\n")
      for (i <- 0 to 4) {dmem(i) := 5.U - i.asUInt}
      state := sRun
    }

    is(sRun) {
      //when(dut.io.icache.req.valid) {
      //  printf(s"INST[%x] => %x, iaddr: %x\n", dut.io.icache.req.bits.addr, mem(iaddr), iaddr)
      //}
      //when(dut.io.dcache.req.valid) {
      //  when(dut.io.dcache.req.bits.mask.orR) {
      //    mem(daddr) := write
      //    printf("MEM[%x] <= %x\n", dut.io.dcache.req.bits.addr, write)
      //  }.otherwise {
      //    printf("MEM[%x] => %x\n", dut.io.dcache.req.bits.addr, mem(daddr))
      //  }
      //}
      //timeout := timeout + 1.U
      //assert(timeout < 100.U)
      //when(dut.io.host.tohost =/= 0.U) {
      //  assert(dut.io.host.tohost === testResults(testType).U,
      //         s"* tohost: %d != ${testResults(testType)} *", dut.io.host.tohost)
      //  stop(); stop()
      //}

      when(!dut.io.icache.wen) {
        printf(s"INST[%x] => %x, iaddr: %x    ", dut.io.icache.addr, imem(iaddr), iaddr)
      }
      //when(true.B) {
      printf("alu_out = %x, ew_alu = %x, reg_wen = %b, reg_write = %x    ", dut.io.debug.alu_out, dut.io.debug.ew_alu, dut.io.debug.regFile_wen, dut.io.debug.regFile_wdata)
      when(dut.io.dcache.wen) {
        dmem(daddr) := write
        printf("MEM[%x] <= %x W\n", dut.io.dcache.addr, write)
      }.otherwise {
        printf("MEM[%x] => %x R\n", dut.io.dcache.addr, dmem(daddr))
      }
      //}
      //timeout := timeout + 1.U
      //assert(timeout < 250.U)
      //when(dut.io.host.tohost =/= 0.U) {
      //  assert(dut.io.host.tohost === testResults(testType).U,
      //         s"* tohost: %d != ${testResults(testType)} *", dut.io.host.tohost)
      //  stop(); stop()
      //}
      when(iaddr ===  Const.PC_START.U + 0x1B.U) {
        assert(dmem(0) === 1.U)
        assert(dmem(1) === 2.U)
        assert(dmem(2) === 3.U)
        assert(dmem(3) === 4.U)
        assert(dmem(4) === 5.U)
        printf("Datapath Test Finishes!\n")
        printf("dem(0) = %x\n", dmem(0))
        printf("dem(1) = %x\n", dmem(1))
        printf("dem(2) = %x\n", dmem(2))
        printf("dem(3) = %x\n", dmem(3))
        printf("dem(4) = %x\n", dmem(4))
        stop(); stop()
      }
    }
  }
}

class DatapathTests extends org.scalatest.FlatSpec {
  //implicit val p = (new MiniConfig).toInstance
  implicit val xlen = 32
  println("###############")
  println("test begins")
  Seq(SortTest) foreach { test =>
    "Datapath" should s"pass $test" in {
      assert(TesterDriver execute (() => new DatapathTester(new Datapath(true))))
    }
  }
}
