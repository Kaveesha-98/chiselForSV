import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

class n_adder(N: Int = 4) extends Module {
	val A, B = IO(Input(SInt(N.W)))
	val ci = IO(Input(UInt(1.W)))

	val S = IO(Output(SInt(N.W)))
	val co = IO(Output(UInt(1.W)))

	val C = Array.fill(N+1){ Wire(UInt(1.W)) }
	C(0) := ci

	/*
		Chisel doesn't support individually assigning values
		to wires in a bus(pro or con? don't know)
	*/
	/*
		tabulate is similar to for loop. It generates element using integers
		0 - (N-1) then bundles them into an array.
		In this case its creates an array of "sum" outputs of the fulladders
		Which is then concatenated to the bus output "S"
	*/
	S := Cat(Seq.tabulate(N)(i => {
		val adder = Module(new fulladder)
		adder.a := A(i).asUInt
		adder.b := B(i).asUInt
		adder.ci := C(i)
		C(i+1) := adder.co
		adder.sum // creating an array of bit, then contatenating using Cat()
	}).reverse).asSInt
	/*
		".reverse" is needed because Cat() function actually works in the wrong order
		it concatenates the bus in the oppesite order.
		Not sure if I'm the one thats thinking in reverse, or if this is a problem that
		really should be solved before releasing. There is a good chance a lot code bases
		use Cat() this way. And solving this might cause alot of code bases to change.
		This looks like a problem that needs to be asked from the creators of Chisel.
	*/

	co := C(N)
}

object n_adder extends App {
	(new stage.ChiselStage).emitVerilog(new n_adder())
}
