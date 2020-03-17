package riscv_5stage

import chisel3._
import chisel3.util._

trait ALUConifg {
  protected val ALUOpWidth: Int = 4
  protected val DataWidth: Int = 32
}
