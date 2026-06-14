package vutils.fsm

import chisel3._
import chisel3.util.DecoupledIO
import scala.collection.mutable.ArrayBuffer

final class ElasticStage[T <: Data, N <: Data] private[fsm] (
  val id: Int,
  val node: N,
  private[fsm] val localReady: Bool,
  private[fsm] val validReg: Bool,
  private[fsm] val bitsReg: T,
  private[fsm] val readyWire: Bool,
  private[fsm] val fireWire: Bool
) {
  def valid: Bool = validReg
  def bits: T     = bitsReg
  def ready: Bool = readyWire
  def fire: Bool  = fireWire

  override def toString: String = s"ElasticStage($id, $node)"
}

final class ElasticMove private[fsm] (
  val name: String,
  private[fsm] val fireWire: Bool
) {
  def fire: Bool = fireWire

  override def toString: String = s"ElasticMove($name)"
}

final class ElasticGraph[T <: Data, N <: Data] private[fsm] (
  val stages: Seq[ElasticStage[T, N]],
  val moves: Seq[ElasticMove]
) {
  def apply(index: Int): ElasticStage[T, N] = stages(index)

  def apply(node: N): ElasticStage[T, N] =
    find(node).getOrElse(throw new NoSuchElementException(s"unknown elastic node: $node"))

  def find(node: N): Option[ElasticStage[T, N]] =
    stages.find(_.node == node)
}

final class ElasticGraphBuilder[T <: Data, N <: Data] private[fsm] (
  gen: T,
  clear: Bool,
  enable: Bool
) {
  final private class StageEdge(
    val from: ElasticStage[T, N],
    val to: ElasticStage[T, N],
    val trigger: Bool,
    val move: ElasticMove,
    val order: Int
  )

  final private class SourceEdge(
    val in: DecoupledIO[T],
    val to: ElasticStage[T, N],
    val trigger: Bool,
    val move: ElasticMove,
    val order: Int
  )

  final private class SinkEdge(
    val from: ElasticStage[T, N],
    val out: DecoupledIO[T],
    val trigger: Bool,
    val move: ElasticMove,
    val order: Int
  )

  private val stages      = ArrayBuffer[ElasticStage[T, N]]()
  private val stageEdges  = ArrayBuffer[StageEdge]()
  private val sourceEdges = ArrayBuffer[SourceEdge]()
  private val sinkEdges   = ArrayBuffer[SinkEdge]()
  private val moves       = ArrayBuffer[ElasticMove]()

  private var order = 0

  private def nextOrder(): Int = {
    val value = order
    order += 1
    value
  }

  private def boolOr(values: Iterable[Bool]): Bool =
    if (values.isEmpty) false.B else values.reduce(_ || _)

  private def nodeName(node: N): String =
    node.toString

  private def outgoingOf(stage: ElasticStage[T, N]): Seq[Either[StageEdge, SinkEdge]] = {
    val stageOut = stageEdges.filter(_.from.id == stage.id).map(edge => Left(edge): Either[StageEdge, SinkEdge])
    val sinkOut  = sinkEdges.filter(_.from.id == stage.id).map(edge => Right(edge): Either[StageEdge, SinkEdge])

    (stageOut ++ sinkOut).sortBy {
      case Left(edge)  => edge.order
      case Right(edge) => edge.order
    }.toSeq
  }

  def stage(node: N, ready: Bool = true.B): ElasticStage[T, N] = {
    require(!stages.exists(_.node == node), s"duplicate elastic node: $node")

    val valid     = RegInit(false.B)
    val bits      = Reg(gen)
    val readyWire = WireDefault(false.B)
    val fireWire  = WireDefault(false.B)

    val s = new ElasticStage[T, N](
      id = stages.length,
      node = node,
      localReady = ready,
      validReg = valid,
      bitsReg = bits,
      readyWire = readyWire,
      fireWire = fireWire
    )

    stages += s
    s
  }

  def source(in: DecoupledIO[T], to: ElasticStage[T, N], trigger: Bool = true.B): ElasticMove = {
    val fire = WireDefault(false.B)
    val move = new ElasticMove(s"source -> ${nodeName(to.node)}", fire)

    sourceEdges += new SourceEdge(in, to, trigger, move, nextOrder())
    moves += move

    move
  }

  def connect(from: ElasticStage[T, N], to: ElasticStage[T, N], trigger: Bool = true.B): ElasticMove = {
    val fire = WireDefault(false.B)
    val move = new ElasticMove(s"${nodeName(from.node)} -> ${nodeName(to.node)}", fire)

    stageEdges += new StageEdge(from, to, trigger, move, nextOrder())
    moves += move

    move
  }

  def sink(from: ElasticStage[T, N], out: DecoupledIO[T], trigger: Bool = true.B): ElasticMove = {
    val fire = WireDefault(false.B)
    val move = new ElasticMove(s"${nodeName(from.node)} -> sink", fire)

    sinkEdges += new SinkEdge(from, out, trigger, move, nextOrder())
    moves += move

    move
  }

  private[fsm] def finish(): ElasticGraph[T, N] = {
    require(stages.nonEmpty, "ElasticGraph requires at least one stage")

    for (edge <- stageEdges)
      require(edge.from.id < edge.to.id, s"ElasticGraph only supports forward DAG/tree edges, got ${edge.from.node} -> ${edge.to.node}")

    for (stage <- stages) {
      val incomingCount = sourceEdges.count(_.to.id == stage.id) + stageEdges.count(_.to.id == stage.id)
      require(incomingCount <= 1, s"ElasticGraph currently supports at most one incoming edge per stage, but ${stage.node} has $incomingCount")
    }

    val canAccept = Seq.fill(stages.length)(WireDefault(false.B))
    val canLeave  = Seq.fill(stages.length)(WireDefault(false.B))

    for (stage <- stages.reverse) {
      var priorTrigger = false.B

      val leaveTerms = outgoingOf(stage).map {
        case Left(edge) =>
          val selected = edge.trigger && !priorTrigger
          val leave    = selected && canAccept(edge.to.id)
          priorTrigger = priorTrigger || edge.trigger
          leave

        case Right(edge) =>
          val selected = edge.trigger && !priorTrigger
          val leave    = selected && edge.out.ready
          priorTrigger = priorTrigger || edge.trigger
          leave
      }

      canLeave(stage.id)  := boolOr(leaveTerms)
      canAccept(stage.id) := stage.localReady && (!stage.valid || canLeave(stage.id))
      stage.readyWire     := canAccept(stage.id)
    }

    for (src <- sourceEdges) {
      src.in.ready      := enable && !clear && src.trigger && canAccept(src.to.id)
      src.move.fireWire := src.in.valid && src.in.ready
    }

    for (sink <- sinkEdges) {
      sink.out.valid := false.B
      sink.out.bits  := 0.U.asTypeOf(gen)
    }

    for (stage <- stages) {
      var priorTrigger = false.B

      val fireTerms = outgoingOf(stage).map {
        case Left(edge) =>
          val selected = edge.trigger && !priorTrigger
          val fire     = enable && !clear && stage.valid && stage.localReady && selected && canAccept(edge.to.id)

          edge.move.fireWire := fire
          priorTrigger = priorTrigger || edge.trigger

          fire

        case Right(edge) =>
          val selected = edge.trigger && !priorTrigger
          val fire     = enable && !clear && stage.valid && stage.localReady && selected && edge.out.ready

          edge.move.fireWire := fire
          edge.out.valid     := stage.valid && stage.localReady && selected && !clear
          edge.out.bits      := stage.bits
          priorTrigger = priorTrigger || edge.trigger

          fire
      }

      stage.fireWire := boolOr(fireTerms)
    }

    for (stage <- stages) {
      val sourceIn = sourceEdges.find(_.to.id == stage.id)
      val stageIn  = stageEdges.find(_.to.id == stage.id)

      val incomingFire = boolOr(sourceIn.map(_.move.fire).toSeq ++ stageIn.map(_.move.fire).toSeq)
      val incomingBits = WireDefault(0.U.asTypeOf(gen))

      sourceIn.foreach { edge =>
        when(edge.move.fire) {
          incomingBits := edge.in.bits
        }
      }

      stageIn.foreach { edge =>
        when(edge.move.fire) {
          incomingBits := edge.from.bits
        }
      }

      when(clear) {
        stage.validReg := false.B
      }.elsewhen(enable) {
        when(incomingFire) {
          stage.validReg := true.B
          stage.bitsReg  := incomingBits
        }.elsewhen(stage.fire) {
          stage.validReg := false.B
        }
      }
    }

    new ElasticGraph[T, N](
      stages = stages.toSeq,
      moves = moves.toSeq
    )
  }
}

trait ElasticGraphSyntax { this: Module =>
  final protected def elastic[T <: Data, E <: ChiselEnum](
    gen: T,
    nodeType: E,
    clear: Bool = false.B,
    enable: Bool = true.B
  )(
    build: ElasticGraphBuilder[T, nodeType.Type] => Unit
  ): ElasticGraph[T, nodeType.Type] = {
    val builder = new ElasticGraphBuilder[T, nodeType.Type](gen, clear, enable)
    build(builder)
    builder.finish()
  }
}
