package example

import vutils._
import vutils.pipe._
import chisel3._
import chisel3.util.Decoupled

class PipelinedAdd(width: Int) extends Module with PipelineModel {
  val io = IO(new Bundle {
    val in    = Flipped(Decoupled(Vec(4, UInt(width.W))))
    val out   = Decoupled(UInt(width.W))
    val flush = Input(Bool())
  })

  val s0 = pipeMap(Vec(4, UInt(width.W)), Vec(2, UInt(width.W)), io.in, io.flush) { in =>
    val out = Wire(Vec(2, UInt(width.W)))
    out(0) := in(0) + in(1)
    out(1) := in(2) + in(3)
    out
  }

  val s1 = pipeMap(Vec(2, UInt(width.W)), UInt(width.W), s0, io.flush) { in =>
    in(0) + in(1)
  }

  io.out <> s1
}

object PipelinedAddExample extends App {
  DesignEmitter.emit(
    gen = new PipelinedAdd(32),
    filename = "pipelined_add",
    target = SystemVerilog,
    info = true,
    lowering = true,
  )
}
