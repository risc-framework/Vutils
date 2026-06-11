package vutils.fsm

import chisel3._
import chisel3.util.DecoupledIO

final class ElasticStage[T <: Data] private[fsm] (
  val id: Int,
  val name: String,
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

  override def toString: String = s"ElasticStage($id, $name)"
}

final class ElasticMove private[fsm] (
  val name: String,
  private[fsm] val fireWire: Bool
) {
  def fire: Bool = fireWire

  override def toString: String = s"ElasticMove($name)"
}

final class ElasticGraph[T <: Data] private[fsm] (
  val stages: Seq[ElasticStage[T]],
  val moves: Seq[ElasticMove]
) {
  def apply(index: Int): ElasticStage[T] = stages(index)

  def find(name: String): Option[ElasticStage[T]] = stages.find(_.name == name)

  def get(name: String): ElasticStage[T] =
    find(name).getOrElse(throw new NoSuchElementException(s"unknown elastic stage: $name"))
}

final class ElasticGraphBuilder[T <: Data] private[fsm] (
  gen: T,
  clear: Bool,
  enable: Bool
) {
  final private class StageEdge(
    val from: ElasticStage[T],
    val to: ElasticStage[T],
    val trigger: Bool,
    val move: ElasticMove,
    val order: Int
  )

  final private class SourceEdge(
    val in: DecoupledIO[T],
    val to: ElasticStage[T],
    val trigger: Bool,
    val move: ElasticMove,
    val order: Int
  )

  final private class SinkEdge(
    val from: ElasticStage[T],
    val out: DecoupledIO[T],
    val trigger: Bool,
    val move: ElasticMove,
    val order: Int
  )

  private val stages      = scala.collection.mutable.ArrayBuffer[ElasticStage[T]]()
  private val stageEdges  = scala.collection.mutable.ArrayBuffer[StageEdge]()
  private val sourceEdges = scala.collection.mutable.ArrayBuffer[SourceEdge]()
  private val sinkEdges   = scala.collection.mutable.ArrayBuffer[SinkEdge]()
  private val moves       = scala.collection.mutable.ArrayBuffer[ElasticMove]()

  private var order = 0

  private def nextOrder(): Int = {
    val value = order
    order += 1
    value
  }

  private def boolOr(values: Iterable[Bool]): Bool =
    if (values.isEmpty) false.B else values.reduce(_ || _)

  private def outgoingOf(stage: ElasticStage[T]): Seq[Either[StageEdge, SinkEdge]] = {
    val stageOut = stageEdges.filter(_.from.id == stage.id).map(edge => Left(edge): Either[StageEdge, SinkEdge])
    val sinkOut  = sinkEdges.filter(_.from.id == stage.id).map(edge => Right(edge): Either[StageEdge, SinkEdge])
    (stageOut ++ sinkOut).sortBy {
      case Left(edge)  => edge.order
      case Right(edge) => edge.order
    }.toSeq
  }

  def stage(name: String, ready: Bool = true.B): ElasticStage[T] = {
    val valid     = RegInit(false.B)
    val bits      = Reg(gen)
    val readyWire = WireDefault(false.B)
    val fireWire  = WireDefault(false.B)

    val stage = new ElasticStage[T](
      id = stages.length,
      name = name,
      localReady = ready,
      validReg = valid,
      bitsReg = bits,
      readyWire = readyWire,
      fireWire = fireWire
    )

    stages += stage
    stage
  }

  def source(in: DecoupledIO[T], to: ElasticStage[T], trigger: Bool = true.B): ElasticMove = {
    val fire = WireDefault(false.B)
    val move = new ElasticMove(s"source -> ${to.name}", fire)

    sourceEdges += new SourceEdge(in, to, trigger, move, nextOrder())
    moves += move

    move
  }

  def connect(from: ElasticStage[T], to: ElasticStage[T], trigger: Bool = true.B): ElasticMove = {
    val fire = WireDefault(false.B)
    val move = new ElasticMove(s"${from.name} -> ${to.name}", fire)

    stageEdges += new StageEdge(from, to, trigger, move, nextOrder())
    moves += move

    move
  }

  def sink(from: ElasticStage[T], out: DecoupledIO[T], trigger: Bool = true.B): ElasticMove = {
    val fire = WireDefault(false.B)
    val move = new ElasticMove(s"${from.name} -> sink", fire)

    sinkEdges += new SinkEdge(from, out, trigger, move, nextOrder())
    moves += move

    move
  }

  private[fsm] def finish(): ElasticGraph[T] = {
    require(stages.nonEmpty, "ElasticGraph requires at least one stage")

    for (edge <- stageEdges)
      require(edge.from.id < edge.to.id, s"ElasticGraph only supports forward DAG/tree edges, got ${edge.from.name} -> ${edge.to.name}")

    for (stage <- stages) {
      val incomingCount = sourceEdges.count(_.to.id == stage.id) + stageEdges.count(_.to.id == stage.id)
      require(incomingCount <= 1, s"ElasticGraph currently supports at most one incoming edge per stage, but ${stage.name} has $incomingCount")
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

    new ElasticGraph[T](
      stages = stages.toSeq,
      moves = moves.toSeq
    )
  }
}

trait ElasticGraphSyntax { this: Module =>
  final protected def elastic[T <: Data](
    gen: T,
    clear: Bool = false.B,
    enable: Bool = true.B
  )(
    build: ElasticGraphBuilder[T] => Unit
  ): ElasticGraph[T] = {
    val builder = new ElasticGraphBuilder[T](gen, clear, enable)
    build(builder)
    builder.finish()
  }
}
