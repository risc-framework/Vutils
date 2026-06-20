package vutils.pipe

import chisel3._
import chisel3.util.Decoupled

class PipelineTransformIO[In <: Data, Out <: Data](inGen: In, outGen: Out) extends Bundle {
  val in    = Flipped(Decoupled(inGen.cloneType))
  val out   = Decoupled(outGen.cloneType)
  val flush = Input(Bool())
}

class PipelineTransform[In <: Data, Out <: Data](
  inGen: In,
  outGen: Out,
  fn: In => Out
) extends Module {
  val io = IO(new PipelineTransformIO(inGen, outGen))

  private val stage  = Module(new PipelineStage(outGen))
  private val mapped = Wire(outGen.cloneType)

  mapped := fn(io.in.bits)

  stage.io.in.valid := io.in.valid
  stage.io.in.bits  := mapped
  stage.io.flush    := io.flush

  io.in.ready := stage.io.in.ready
  io.out <> stage.io.out
}
