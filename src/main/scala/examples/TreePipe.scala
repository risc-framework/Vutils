package examples

import vutils._
import vutils.fsm.ElasticGraphSyntax
import chisel3._
import chisel3.util.Decoupled

object TreeNode extends ChiselEnum {
  val A, B1, B2, C, D = Value
}

class TreeReq extends Bundle {
  val data = UInt(32.W)
  val sel  = UInt(1.W)
}

class TreePipe extends Module with ElasticGraphSyntax {
  val io = IO(new Bundle {
    val in   = Flipped(Decoupled(new TreeReq))
    val outC = Decoupled(new TreeReq)
    val outD = Decoupled(new TreeReq)

    val clear  = Input(Bool())
    val enable = Input(Bool())

    val b1Stuck = Input(Bool())
    val b2Stuck = Input(Bool())

    val aValid  = Output(Bool())
    val b1Valid = Output(Bool())
    val b2Valid = Output(Bool())
    val cValid  = Output(Bool())
    val dValid  = Output(Bool())
  })

  private val p = elastic(new TreeReq, TreeNode.A, clear = io.clear, enable = io.enable) { g =>
    import g._

    val A  = stage(TreeNode.A)
    val B1 = stage(TreeNode.B1, ready = !io.b1Stuck)
    val B2 = stage(TreeNode.B2, ready = !io.b2Stuck)
    val C  = stage(TreeNode.C)
    val D  = stage(TreeNode.D)

    source(io.in, A)

    connect(A, B1, trigger = A.bits.sel === 0.U)
    connect(A, B2, trigger = A.bits.sel === 1.U)

    connect(B1, C)
    connect(B2, D)

    sink(C, io.outC)
    sink(D, io.outD)
  }

  io.aValid  := p(TreeNode.A).valid
  io.b1Valid := p(TreeNode.B1).valid
  io.b2Valid := p(TreeNode.B2).valid
  io.cValid  := p(TreeNode.C).valid
  io.dValid  := p(TreeNode.D).valid
}

object TreePipeExample extends App {
  DesignEmitter.emit(
    gen = new TreePipe,
    filename = "tree_pipe",
    target = SystemVerilog,
    info = true,
    lowering = true,
  )
}
