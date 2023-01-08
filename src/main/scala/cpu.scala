import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum
import os.stat

class register(WIDTH:Int=8) extends Module {
  val io = IO(new Bundle{
    val en = Input(Bool())
    val in = Input(SInt(WIDTH.W))
    val out = Output(SInt(WIDTH.W))
  })

  val reg = RegInit(0.S(WIDTH.W))
  when (io.en) { reg := io.in}
  io.out := reg
}


class cpu(NUM_GPR:Int = 8, WIDTH:Int = 8) extends Module {
    val io = IO(new Bundle {
        val start = Input(UInt(1.W))
        val idle = Output(UInt(1.W))
        // dout, din are named wrt RAM, not CPU
        val iram_dout = Input(UInt(16.W))
        val dram_dout = Input(UInt(WIDTH.W))
    
        val iram_addr, dram_din, dram_addr = Output(UInt(WIDTH.W))
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

  // W-bit processor: All registers are W bits
  val bus_a, bus_b, alu_out, ar, jr, im, pc, pc_next, di = Wire(SInt(WIDTH.W))
  val opcode, rd, ra, rb = Wire(UInt(4.W))
  val reg_en = Wire(Vec(NUM_ADDRESSIBLE_REGISTERS,UInt(1.W)))

  //*** ALU (Arithmetic Logic Unit)

  val W_ALU_SEL = 3;
  val alu_sel = Wire(UInt(W_ALU_SEL.W))

  val alu = Module(new comb_alu(WIDTH=WIDTH))
  alu.bus_a := bus_a
  alu.bus_b := bus_b
  alu.alu_sel := alu_sel
  alu_out := alu.alu_out

  //*** GPR (General Purpose Registers)

  val gpr = VecInit.fill(NUM_GPR)(0.S(WIDTH.W))
  val gpr_array = for (i <- 0 until NUM_GPR) yield {
    val REG = Module(new register(WIDTH))
    REG.io.en := reg_en(i+6)
    REG.io.in := alu_out
    gpr(i) := REG.io.out
  }

  //*** Memory Control

  io.iram_addr := pc_next.asUInt
  rb := io.iram_dout(15,12)
  ra := io.iram_dout(11,8)
  rd := io.iram_dout(7,4)
  opcode := io.iram_dout(3,0)
  im := Cat(ra,rb).asSInt

  val AR = Module(new register(WIDTH))
  AR.io.en := reg_en(registerAddresses.R_AR.asUInt)
  AR.io.in := alu_out
  ar := AR.io.out

  val JR = Module(new register(WIDTH))
  JR.io.en := reg_en(registerAddresses.R_JR.asUInt)
  JR.io.in := alu_out
  jr := JR.io.out

  di := io.dram_dout.asSInt
  io.dram_addr := ar.asUInt
  io.dram_din := alu_out.asUInt


  //*** Bus
  val bus_a_sel, bus_b_sel = Wire(UInt(W_REG_ADDR.W))

  when (bus_a_sel === 0.U) {bus_a := 0.S}
  .elsewhen(bus_a_sel === 1.U) {bus_a := 1.S}
  .elsewhen(bus_a_sel === registerAddresses.R_DI.asUInt) { bus_a := di}
  .elsewhen(bus_a_sel === registerAddresses.R_IM.asUInt) { bus_a := im}
  .elsewhen(bus_a_sel === registerAddresses.R_AR.asUInt) { bus_a := ar}
  .elsewhen(bus_a_sel === registerAddresses.R_JR.asUInt) { bus_a := jr}
  .otherwise {bus_a := gpr(bus_a_sel-6.U)}
  
  when (bus_b_sel === 0.U) {bus_b := 0.S}
  .elsewhen(bus_b_sel === 1.U) {bus_b := 1.S}
  .elsewhen(bus_b_sel === registerAddresses.R_DI.asUInt) { bus_b := di}
  .elsewhen(bus_b_sel === registerAddresses.R_IM.asUInt) { bus_b := im}
  .elsewhen(bus_b_sel === registerAddresses.R_AR.asUInt) { bus_b := ar}
  .elsewhen(bus_b_sel === registerAddresses.R_JR.asUInt) { bus_b := jr}
  .otherwise {bus_b := gpr(bus_b_sel-6.U)}

  //*** State Machine: (Fetch, Decode, Execute)

  object  cpuStates extends ChiselEnum {
    val S_IDLE, S_FETCH, S_DECODE_EXECUTE= Value
  }

  val state = RegInit(cpuStates.S_IDLE)

  switch (state) {
    is (cpuStates.S_IDLE){
      when(io.start.asBool){
        state := cpuStates.S_FETCH
      }.otherwise{
        state := cpuStates.S_IDLE
      }
    }
    is (cpuStates.S_FETCH){
      state := cpuStates.S_DECODE_EXECUTE
    }
    is (cpuStates.S_DECODE_EXECUTE){
      when(opcode === opcodes.I_END.asUInt){
        state := cpuStates.S_IDLE
      }.otherwise{
        state := cpuStates.S_FETCH
      }
    }
  }


  //*** PC (Program Counter)
  // Here, pc holds addr of current instruction.
  //       pc_next = iram_addr = address of next instruction
  //       jump_success register tells to branch to JDR in the next clock cycle

  val pc_en, jump_success, jump_success_next = Wire(Bool())

  val PC = Module(new register(WIDTH))
  PC.io.en := pc_en
  PC.io.in := pc_next
  pc := PC.io.out

  val JS = Module(new register(1))
  JS.io.en := pc_en
  JS.io.in := jump_success_next.asSInt
  jump_success := JS.io.out.asBool

  io.idle := state === cpuStates.S_IDLE
  pc_en := state === cpuStates.S_DECODE_EXECUTE

  //*** Instruction Decoder
    alu_sel := 0.U
    reg_en.map(_ := 0.U)
    io.dram_write := 0.U
    jump_success_next := false.B
    bus_a_sel := ra
    bus_b_sel := rb
    pc_next := Mux(jump_success, jr, pc + 1.S)

    when(state === cpuStates.S_DECODE_EXECUTE){
      switch(opcodes(opcode)){
        is(opcodes.I_END) {pc_next := 0.S}
        is(opcodes.I_LDM) {io.dram_write := 0.U} // DI <- DRAM[AR]
        is(opcodes.I_STM) {io.dram_write := 1.U} // DRAM[AR] <- A[ra]  (alu passes a by default)
        is(opcodes.I_MVI) { // R[rd] <- IM
          bus_a_sel := registerAddresses.R_IM.asUInt  // bus_a  <- IM
          reg_en(rd) := 1.U  // AR[rd] <- bus_a (alu passes a by default)
        }
        is(opcodes.I_MVR) { reg_en(rd) := 1.U} // R[rd] <- A[ra] (alu passes a by default)
        is(opcodes.I_BEQ) {  // if R[ra] != R[rb], pc_next = JR in next clock
          alu_sel := opcodes.I_SUB.asUInt
          jump_success_next := (alu_out === 0.S)
        }
        is(opcodes.I_BLT) {  // if R[ra] <  R[rb], pc_next = JR in next clock
          alu_sel := opcodes.I_SUB.asUInt
          jump_success_next := (alu_out < 0.S)
        }
      }
    }  
}

object cpu extends App {
    (new stage.ChiselStage).emitVerilog(new cpu())
}