import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._

import Chisel.log2Ceil

class p2s(val N:Int = 8) extends Module {
    val s_ready, p_valid    = IO(Input(Bool()))
    val p_data              = IO(Input(UInt(N.W)))
    val p_ready, s_valid    = IO(Output(Bool()))
    val s_data              = IO(Output(UInt(1.W)))

    /**
      * rx - waiting for parallel data from input serialize
      * tx - serializing and transmitting for the parallel data(p_data)
      */
    val rx :: tx :: Nil = Enum(2)
    val count = RegInit(0.U(log2Ceil(N).W))

    /**
      * When reset asserted state(dedicated register to track FSM) is set
      * set to rx.
      * 
      * p_valid indicated valid p_data to serialize.
      * 
      * It takes N cycles of s_ready to be asserted to finish transmitting. 
      */
    val state = RegInit(rx)
    switch(state){
        is(rx) { state := Mux(p_valid, tx, rx) }
        is(tx) { state := Mux(count === (N-1).U && s_ready, rx, tx) }
    }

    /**
      * p_data is stored in a shift register to serialize
      */
    val shift_reg = Reg(UInt(N.W))

    s_data  := shift_reg(0)
    // can't accept new p_data until old p_data has finished transmitting
    p_ready := (state === rx)
    s_valid := (state === tx)

    switch(state) {
        is(rx) { shift_reg := p_data }
        is(tx) {
            when(s_ready) {
                shift_reg := shift_reg >> 1
                count := Mux(count =/= (N-1).U, count + 1.U, 0.U);
            }
        }
    }
}

object p2s extends  App {
    (new stage.ChiselStage).emitVerilog(new p2s)
}