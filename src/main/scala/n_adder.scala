import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

class n_adder(N: Int = 4) extends Module {
	val A, B = IO(Input(SInt(N.W)))
	val ci = IO(Input(UInt(1.W)))

	val S = IO(Output(SInt(N.W)))
	val co = IO(Output(UInt(1.W)))

	// instantiating adders and connecting bus A and B bits
	val adders = Seq.tabulate(N)(i => (Module(new fulladder), A(i), B(i)))
		.map{ case(adder, a_i, b_i) => {adder.a := a_i; adder.b := b_i; adder} }

	// connecting the carry bits and extracting the final carry bit
	co := adders.foldLeft(ci)((carr_in, adder) => { adder.ci := carr_in; adder.co })
	// getting the sum bit(without ".reverse" it bundles the bits in the wrong direction)
	S := Cat(adders.map(_.sum).reverse).asSInt
}

object n_adder extends App {
	(new stage.ChiselStage).emitVerilog(new n_adder())
}
