import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum

class cpu(NUM_GPR:Int = 8, W:Int = 8) extends Module {
    val io = IO(new Bundle {
        val clk, rstn, start = Input(UInt(1.W))
        val idle = Output(UInt(1.W))
        // dout, din are named wrt RAM, not CPU
        val iram_dout = Input(UInt(16.W))
        val dram_dout = Input(UInt(16.W))
    
        val iram_addr, dram_din, dram_addr = Output(UInt(W.W))
        val dram_write = Output(UInt(1.W))
    })

    val NUM_ADDRESSIBLE_REGISTERS = 6 + NUM_GPR
    val W_REG_ADDR = log2Ceil(NUM_ADDRESSIBLE_REGISTERS)

    // Machine code encodings for instruction opcodes
    object opcodes extends ChiselEnum {
        val I_END, I_ADD, I_SUB, I_MUL, I_DV2, 
                       I_LDM, I_STM, I_MVR, I_MVI, I_BEQ, I_BLT = Value
    }

  // Register addressing
  object registerAddresses extends ChiselEnum {
    val R_DI = Value(2.U)
    val R_IM, R_AR, R_JR = Value
    val ukn = Value(NUM_ADDRESSIBLE_REGISTERS.U) // to makesure the inferred width is "W_REG_ADDR"
  }

  
}

object cpu extends App {
    (new stage.ChiselStage).emitVerilog(new cpu())
}