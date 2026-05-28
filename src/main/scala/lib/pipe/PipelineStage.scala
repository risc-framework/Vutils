package vutils.pipe

import chisel3._
import chisel3.util.Decoupled

class PipelineStageIO[T <: Data](gen: T) extends Bundle {
  val in    = Flipped(Decoupled(gen.cloneType))
  val out   = Decoupled(gen.cloneType)
  val flush = Input(Bool())
}

class PipelineStage[T <: Data](gen: T) extends Module {
  val io = IO(new PipelineStageIO(gen))

  private val validReg = RegInit(false.B)
  private val bitsReg  = Reg(gen.cloneType)

  io.in.ready  := !io.flush && (!validReg || io.out.ready)
  io.out.valid := validReg && !io.flush
  io.out.bits  := bitsReg

  when(io.flush) {
    validReg := false.B
  }.elsewhen(io.in.fire && io.out.fire) {
    validReg := true.B
  }.elsewhen(io.in.fire) {
    validReg := true.B
  }.elsewhen(io.out.fire) {
    validReg := false.B
  }

  when(io.in.fire) {
    bitsReg := io.in.bits
  }
}
