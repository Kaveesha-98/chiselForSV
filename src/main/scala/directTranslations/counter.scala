import chisel3._
import chisel3.util._

class counter(N: Int = 8) extends Module {
	val incr = IO(Input(Bool()))
	val count_reg = IO(Output(UInt(N.W)))

	// create a register which is set to zero when reset is asserted
	val internal_count_reg = RegInit(0.U(N.W))

	when (incr) {internal_count_reg := internal_count_reg + 1.U}
	count_reg := internal_count_reg

	/*
		Alternative code without using a new vaiable name
		count_reg := {
			val count_reg = RegInit(0.U(N.W))
			when (incr) { count_reg := count_reg + 1.U }
			count_reg
		}
	
		chisel doesn't support defining registers as I/O. The
		above code inside curly brackets creates the register
		and returns it, which then is assigned to count_reg
	*/
}

object counter extends App {
	(new stage.ChiselStage).emitVerilog(new counter())
}
