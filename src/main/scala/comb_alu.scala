import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

class comb_alu(WIDTH: Int = 8) extends Module {
	val W_ALU_SEL = 3

	val bus_a, bus_b = IO(Input(SInt(WIDTH.W)))
	val alu_out = IO(Output(SInt(WIDTH.W)))
	val alu_sel = IO(Input(UInt(W_ALU_SEL.W)))
	val zero, negative = IO(Output(Bool())) // converts to 1-bit wire, but is more contexually correct here

	alu_out := MuxLookup(alu_sel, bus_a, Seq(
		"b001".U -> (bus_a + bus_b),
		"b010".U -> (bus_a - bus_b),
		"b011".U -> (bus_a * bus_b),
		"b100".U -> (bus_a/2.S)))

	zero := alu_out === 0.S
	negative := alu_out < 0.S
}

object comb_alu extends App {
	(new stage.ChiselStage).emitVerilog(new comb_alu())
}
