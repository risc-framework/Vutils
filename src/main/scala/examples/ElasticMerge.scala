package examples

import chisel3._
import chisel3.util.Decoupled
import vutils._
import vutils.fsm.ElasticGraphSyntax

object MergePipeNode extends ChiselEnum {
  val M, B = Value
}

class ElasticMergeExample extends Module with ElasticGraphSyntax {
  val io = IO(new Bundle {
    val clear  = Input(Bool())
    val in0    = Flipped(Decoupled(UInt(32.W)))
    val in1    = Flipped(Decoupled(UInt(32.W)))
    val out    = Decoupled(UInt(32.W))
    val mCount = Output(UInt(3.W))
  })

  private val pipe = elastic(UInt(32.W), MergePipeNode.M, clear = io.clear) { g =>
    import g._

    val M = merge(MergePipeNode.M, depth = 4)
    val B = stage(MergePipeNode.B)

    source(io.in0, M)
    source(io.in1, M)

    connect(M, B)
    sink(B, io.out)
  }

  io.mCount := pipe(MergePipeNode.M).count
}

object EmitElasticMergeExample extends App {
  DesignEmitter.emit(
    gen = new ElasticMergeExample,
    filename = "elastic_merge_example",
    target = SystemVerilog,
    info = true,
    lowering = true,
  )
}
