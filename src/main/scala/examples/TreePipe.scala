package examples

import vutils.fsm.ElasticGraphSyntax
import vutils._
import chisel3._
import chisel3.util.Decoupled

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

  private val p = elastic(new TreeReq, clear = io.clear, enable = io.enable) { g =>
    import g._

    val A  = stage("A")
    val B1 = stage("B1", ready = !io.b1Stuck)
    val B2 = stage("B2", ready = !io.b2Stuck)
    val C  = stage("C")
    val D  = stage("D")

    source(io.in, A)

    connect(A, B1, trigger = A.bits.sel === 0.U)
    connect(A, B2, trigger = A.bits.sel === 1.U)

    connect(B1, C)
    connect(B2, D)

    sink(C, io.outC)
    sink(D, io.outD)
  }

  io.aValid  := p.get("A").valid
  io.b1Valid := p.get("B1").valid
  io.b2Valid := p.get("B2").valid
  io.cValid  := p.get("C").valid
  io.dValid  := p.get("D").valid
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
