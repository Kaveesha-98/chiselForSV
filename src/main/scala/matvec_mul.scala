import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

import Chisel.log2Ceil
import scala.math.pow

class matvec_mul(
    val R:Int =8, 
    val C:Int =8, 
    val W_X:Int =8, 
    val W_K:Int =8
) extends Module {
    val DEPTH = log2Ceil(C)
    val W_M = W_X + W_K
    val W_Y = W_M + DEPTH
    /**
      * Functionality: Once inputs are presented, elementwise multiplication is performed and the
      * results are written to registers for accumulation. Results are pairwise added the 
      * generated results are written to registers. These intermediate results are then pairwise
      * added again. This contniues until only one result remain. This is the accumilated result.
      * 
      * Whole process takes DEPTH number of clock cycles 
      */

    val x = IO(Input(Vec(C, UInt(W_X.W))))
    val k = IO(Input(Vec(R, x.cloneType)))
    val y = IO(Output(Vec(R, UInt(W_Y.W))))

    // Accumulation takes DEPTH number of cycles. Intermediate results are stored in registers
    val tree = Seq.fill(R){ Seq.tabulate(DEPTH+1)(d => RegInit(VecInit.fill(pow(2, d).toInt)(0.U(W_Y.W)))) }
    
    // pair wise addition of intermediate results
    (0 until R).flatMap( r => Seq.tabulate(DEPTH)(d => (r, d)) )
    .foreach{ case (r, d) => (0 until tree(r)(d).length).foreach(i => tree(r)(d)(i) := tree(r)(d+1)(2*i) +& tree(r)(d+1)(2*i + 1)) }

    // performing element-wise multiplication(there extra registers when C != a power of two)
    (0 until R).foreach( r => (0 until tree(r)(DEPTH).length).foreach(c => tree(r)(DEPTH)(c) := (if (c < C) {k(r)(c) * x(c)} else { 0.U })) )

    // assigning results to output
    (0 until R).foreach( r => y(r) := tree(r)(0)(0))
}

object matvec_mul extends  App {
    (new stage.ChiselStage).emitVerilog(new matvec_mul)
}