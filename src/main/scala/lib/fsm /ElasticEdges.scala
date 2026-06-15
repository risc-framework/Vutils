package vutils.fsm

import chisel3._
import chisel3.util.DecoupledIO

private[fsm] final class ElasticSourceEdge[T <: Data, N <: Data](
  val in: DecoupledIO[T],
  val to: ElasticStage[T, N],
  val trigger: Bool,
  val mapFn: T => Unit,
  val move: ElasticMove,
  val order: Int
) {
  def assignBits(out: T): Unit = {
    out := in.bits
    mapFn(out)
  }
}

private[fsm] final class ElasticStageEdge[T <: Data, N <: Data](
  val from: ElasticStage[T, N],
  val to: ElasticStage[T, N],
  val trigger: Bool,
  val mapFn: T => Unit,
  val move: ElasticMove,
  val wantWire: Bool,
  val order: Int
) {
  def assignBits(out: T): Unit = {
    out := from.bits
    mapFn(out)
  }
}

private[fsm] trait ElasticRequestEdgeLike[T <: Data, N <: Data] {
  def from: ElasticStage[T, N]
  def to: ElasticStage[T, N]
  def trigger: Bool
  def move: ElasticMove
  def order: Int

  def reqReady: Bool
  def init(): Unit
  def drive(active: Bool, selected: Bool, canAcceptTo: Bool): Bool
  def assignBits(out: T): Unit
}

private[fsm] final class ElasticRequestEdge[T <: Data, N <: Data, R <: Data](
  val from: ElasticStage[T, N],
  val req: DecoupledIO[R],
  val to: ElasticStage[T, N],
  val trigger: Bool,
  val reqMapFn: R => Unit,
  val waitMapFn: T => Unit,
  val move: ElasticMove,
  val order: Int
) extends ElasticRequestEdgeLike[T, N] {
  def reqReady: Bool = req.ready

  def init(): Unit = {
    req.valid := false.B
    req.bits := 0.U.asTypeOf(req.bits)
    move.fireWire := false.B
  }

  def drive(active: Bool, selected: Bool, canAcceptTo: Bool): Bool = {
    val bits = WireDefault(0.U.asTypeOf(req.bits))

    reqMapFn(bits)

    req.valid := active && selected && canAcceptTo
    req.bits := bits

    val fire = req.valid && req.ready

    move.fireWire := fire
    fire
  }

  def assignBits(out: T): Unit = {
    out := from.bits
    waitMapFn(out)
  }
}

private[fsm] trait ElasticResponseEdgeLike[T <: Data, N <: Data] {
  def from: ElasticStage[T, N]
  def to: ElasticStage[T, N]
  def trigger: Bool
  def move: ElasticMove
  def wantWire: Bool
  def order: Int

  def respValid: Bool
  def init(): Unit
  def driveNonMerge(active: Bool, selected: Bool, canAcceptTo: Bool): Bool
  def driveWant(active: Bool, selected: Bool): Unit
  def driveMerge(canAcceptTo: Bool, priorCandidate: Bool): Bool
  def assignBits(out: T): Unit
}

private[fsm] final class ElasticResponseEdge[T <: Data, N <: Data, R <: Data](
  val from: ElasticStage[T, N],
  val resp: DecoupledIO[R],
  val to: ElasticStage[T, N],
  val trigger: Bool,
  val mapFn: (T, R) => Unit,
  val move: ElasticMove,
  val wantWire: Bool,
  val order: Int
) extends ElasticResponseEdgeLike[T, N] {
  def respValid: Bool = resp.valid

  def init(): Unit = {
    resp.ready := false.B
    move.fireWire := false.B
    wantWire := false.B
  }

  def driveNonMerge(active: Bool, selected: Bool, canAcceptTo: Bool): Bool = {
    resp.ready := active && selected && canAcceptTo

    val fire = resp.valid && resp.ready

    move.fireWire := fire
    fire
  }

  def driveWant(active: Bool, selected: Bool): Unit = {
    wantWire := active && selected && resp.valid
  }

  def driveMerge(canAcceptTo: Bool, priorCandidate: Bool): Bool = {
    val fire = wantWire && canAcceptTo && !priorCandidate

    resp.ready := fire
    move.fireWire := fire

    fire
  }

  def assignBits(out: T): Unit = {
    out := from.bits
    mapFn(out, resp.bits)
  }
}

private[fsm] trait ElasticSinkEdgeLike[T <: Data, N <: Data] {
  def from: ElasticStage[T, N]
  def trigger: Bool
  def move: ElasticMove
  def order: Int

  def ready: Bool
  def init(): Unit
  def drive(active: Bool, selected: Bool): Bool
}

private[fsm] final class ElasticSinkEdge[T <: Data, N <: Data, R <: Data](
  val from: ElasticStage[T, N],
  val out: DecoupledIO[R],
  val trigger: Bool,
  val mapFn: R => Unit,
  val move: ElasticMove,
  val order: Int
) extends ElasticSinkEdgeLike[T, N] {
  def ready: Bool = out.ready

  def init(): Unit = {
    out.valid := false.B
    out.bits := 0.U.asTypeOf(out.bits)
    move.fireWire := false.B
  }

  def drive(active: Bool, selected: Bool): Bool = {
    val bits = WireDefault(0.U.asTypeOf(out.bits))

    mapFn(bits)

    out.valid := active && selected
    out.bits := bits

    val fire = out.valid && out.ready

    move.fireWire := fire
    fire
  }
}
