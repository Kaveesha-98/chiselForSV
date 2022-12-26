import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

class fulladder extends Module {
	/* Chisel supports defining I/O in any part of code */
	val a, b, ci = IO(Input(UInt(1.W)))
	val sum, co = IO(Output(UInt(1.W)))

	/*
		I have only shown two ways of defining combinational assignment
			1. Decalring wires and assigning combinational logic later
			2. Declaring and assigning combinational logic in the same line
	*/
	val wire_1, wire_2 = Wire(UInt(1.W))

	wire_1 := a | b
	wire_2 := wire_1 & ci

	val wire_3 = a & b

	/* There are no always blocks in Chisel(Pro or Con? Don't know yet) */
	co := wire_2 | wire_3
	sum := wire_1 ^ ci

	/*
		Third type of assignment of combinational logic requires conditional statements
		eg:
		val a = WireDefault(1.U(3.W)) // 1 is the default value of the wire

		when(b === 0.U) {
			a := 3.U
		}

		switch(c) {
			is(0.U) { a := 1.U }
			is(2.U) { a := 2.U }
		}
		// if none of the conditional statements match 'a' will have the default value
	*/
}

object fulladder extends App {
	(new chisel3.stage.ChiselStage).emitVerilog(new fulladder())
}
