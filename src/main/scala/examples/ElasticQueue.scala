package examples

import chisel3._
import chisel3.util.Decoupled
import vutils._
import vutils.fsm.ElasticGraphSyntax

object QueuePipeNode extends ChiselEnum {
  val A, Q, B = Value
}

class ElasticQueueExample extends Module with ElasticGraphSyntax {
  val io = IO(new Bundle {
    val clear  = Input(Bool())
    val in     = Flipped(Decoupled(UInt(32.W)))
    val out    = Decoupled(UInt(32.W))
    val qCount = Output(UInt(3.W))
  })

  private val pipe = elastic(UInt(32.W), QueuePipeNode.A, clear = io.clear) { g =>
    import g._

    val A = stage(QueuePipeNode.A)
    val Q = queue(QueuePipeNode.Q, depth = 4)
    val B = stage(QueuePipeNode.B)

    source(io.in -> A)

    connect(A -> Q)
    connect(Q -> B)

    sink(B -> io.out)
  }

  io.qCount := pipe(QueuePipeNode.Q).count
}

object EmitElasticQueueExample extends App {
  DesignEmitter.emit(
    gen = new ElasticQueueExample,
    filename = "elastic_queue_example",
    target = SystemVerilog,
    info = true,
    lowering = true,
  )
}
