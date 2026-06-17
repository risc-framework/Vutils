package examples

import chisel3._
import chisel3.util.Decoupled
import vutils._
import vutils.fsm.ElasticGraphSyntax

object LinePipeNode extends ChiselEnum {
  val A, B = Value
}

class ElasticLineExample extends Module with ElasticGraphSyntax {
  val io = IO(new Bundle {
    val clear  = Input(Bool())
    val in     = Flipped(Decoupled(UInt(32.W)))
    val out    = Decoupled(UInt(32.W))
    val aValid = Output(Bool())
    val bValid = Output(Bool())
  })

  private val pipe = elastic(UInt(32.W), LinePipeNode.A, clear = io.clear) { g =>
    import g._

    val A = stage(LinePipeNode.A)
    val B = stage(LinePipeNode.B)

    source(io.in -> A)

    connect(A -> B)

    sink(B -> io.out)
  }

  io.aValid := pipe(LinePipeNode.A).valid
  io.bValid := pipe(LinePipeNode.B).valid
}

object EmitElasticLineExample extends App {
  DesignEmitter.emit(
    gen = new ElasticLineExample,
    filename = "elastic_line_example",
    target = SystemVerilog,
    info = true,
    lowering = true,
  )
}
