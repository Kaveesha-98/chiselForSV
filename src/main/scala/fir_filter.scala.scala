import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

class fir_filter(
    B: Seq[Int],
    val WIDTH_X: Int = 8,
    val WIDTH_B: Int = 3
    ) extends Module {

    val WIDTH_M = WIDTH_X + WIDTH_B
    val WIDTH_Y = WIDTH_M + B.size + 1
    val x = IO(Input(SInt(WIDTH_X.W)))
    val y = IO(Output(SInt(WIDTH_Y.W)))

    y := Seq.fill(B.size - 1)(RegInit(0.S(WIDTH_X.W))) // initiating registers that reset to zero
        .scanLeft(RegNext(x, 0.S(WIDTH_X.W)))((priv, curr) => {curr := priv; curr}) // connecting them
        .zip(B.map(_.S(WIDTH_B.W))) // creates an array(software data structure) of (register, coefficient)
        .map(reg_coeff => reg_coeff._1 * reg_coeff._2) // getting patrial products
        .reduce(_ +& _) // accumilating the sums

}

object fir_filter extends  App {
    (new stage.ChiselStage).emitVerilog(new fir_filter(Seq(1, 2, 3)))
}