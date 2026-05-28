package vutils.pipe

import chisel3._
import chisel3.util.DecoupledIO

trait PipelineModel {
  this: Module =>
  protected def pipe[T <: Data](
    gen: T,
    in: DecoupledIO[T],
    flush: Bool = false.B
  ): DecoupledIO[T] = {
    val stage = Module(new PipelineStage(gen))

    stage.io.in <> in
    stage.io.flush := flush

    stage.io.out
  }

  protected def pipeN[T <: Data](
    gen: T,
    in: DecoupledIO[T],
    n: Int,
    flush: Bool = false.B
  ): DecoupledIO[T] = {
    require(n >= 0, s"pipeN stage count must be non-negative, got $n")

    var current = in

    for (i <- 0 until n)
      current = pipe(gen, current, flush)

    current
  }

  protected def pipeMap[In <: Data, Out <: Data](
    inGen: In,
    outGen: Out,
    in: DecoupledIO[In],
    flush: Bool = false.B
  )(
    fn: In => Out
  ): DecoupledIO[Out] = {
    val stage = Module(new PipelineTransform(inGen, outGen, fn))

    stage.io.in <> in
    stage.io.flush := flush

    stage.io.out
  }
}
