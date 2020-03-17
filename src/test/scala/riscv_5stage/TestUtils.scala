// See LICENSE for license details.

package riscv_5stage

import chisel3._
import chisel3.util._
import chisel3.testers._
import scala.reflect.ClassTag
import scala.concurrent.{Future, Await, ExecutionContext}



trait TestUtils {

  def toBigInt(x: Int) = (BigInt(x >>> 1) << 1) | (x & 0x1)

  val alu_op: Seq[UInt]  = Seq(
    0.U(4.W),//ALU_ADD  
    1.U(4.W),//ALU_SLL 
    4.U(4.W),//ALU_XOR 
    6.U(4.W),//ALU_OR 
    7.U(4.W),//ALU_AND 
    5.U(4.W),//ALU_SRL 
    //8.U(4.W)//BitPat("b1000"),//ALU_SEQ 
    //9.U(4.W)//BitPat("b1001"),//ALU_SNE 
    10.U(4.W),//ALU_SUB 
    11.U(4.W),//ALU_SRA 
    12.U(4.W),//ALU_SLT 
    13.U(4.W),//ALU_SGE 
    14.U(4.W),//ALU_SLTU
    15.U(4.W)//ALU_SGEU
  )

  
}

