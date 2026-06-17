package vutils.fsm

import chisel3._
import chisel3.util.DecoupledIO
import scala.collection.mutable.ArrayBuffer

final class ElasticGraphBuilder[T <: Data, N <: Data] private[fsm] (
  gen: T,
  clear: Bool,
  enable: Bool
) {
  final private class StageStorage(
    val stage: ElasticStage[T, N],
    val validReg: Option[Bool],
    val bitsReg: Option[T],
    val queue: Option[ElasticQueue[T]]
  )

  sealed abstract private class OutgoingRef {
    def order: Int
    def trigger: Bool
    def leave(selected: Bool, canAccept: Seq[Bool]): Bool
    def drive(active: Bool, selected: Bool, canAccept: Seq[Bool]): Bool
  }

  final private class StageOut(val edge: ElasticStageEdge[T, N]) extends OutgoingRef {
    def order: Int    = edge.order
    def trigger: Bool = edge.trigger

    def leave(selected: Bool, canAccept: Seq[Bool]): Bool =
      if (edge.to.isMerge) selected && edge.move.fire else selected && canAccept(edge.to.id)

    def drive(active: Bool, selected: Bool, canAccept: Seq[Bool]): Bool = {
      edge.wantWire := active && selected

      if (!edge.to.isMerge) {
        edge.move.fireWire := edge.wantWire && canAccept(edge.to.id)
      }

      edge.move.fire
    }
  }

  final private class RequestOut(val edge: ElasticRequestEdgeLike[T, N]) extends OutgoingRef {
    def order: Int    = edge.order
    def trigger: Bool = edge.trigger

    def leave(selected: Bool, canAccept: Seq[Bool]): Bool =
      selected && edge.reqReady && canAccept(edge.to.id)

    def drive(active: Bool, selected: Bool, canAccept: Seq[Bool]): Bool =
      edge.drive(active, selected, canAccept(edge.to.id))
  }

  final private class ResponseOut(val edge: ElasticResponseEdgeLike[T, N]) extends OutgoingRef {
    def order: Int    = edge.order
    def trigger: Bool = edge.trigger

    def leave(selected: Bool, canAccept: Seq[Bool]): Bool =
      if (edge.to.isMerge) selected && edge.move.fire else selected && edge.respValid && canAccept(edge.to.id)

    def drive(active: Bool, selected: Bool, canAccept: Seq[Bool]): Bool = {
      if (edge.to.isMerge) {
        edge.driveWant(active, selected)
      } else {
        edge.driveNonMerge(active, selected, canAccept(edge.to.id))
      }

      edge.move.fire
    }
  }

  final private class SinkOut(val edge: ElasticSinkEdgeLike[T, N]) extends OutgoingRef {
    def order: Int    = edge.order
    def trigger: Bool = edge.trigger

    def leave(selected: Bool, canAccept: Seq[Bool]): Bool =
      selected && edge.ready

    def drive(active: Bool, selected: Bool, canAccept: Seq[Bool]): Bool =
      edge.drive(active, selected)
  }

  sealed abstract private class IncomingRef {
    def order: Int
    def move: ElasticMove
    def assignBits(out: T): Unit
    def driveMerge(canAccept: Bool, priorCandidate: Bool, allowed: Bool): Bool
  }

  final private class SourceIn(val edge: ElasticSourceEdge[T, N]) extends IncomingRef {
    def order: Int        = edge.order
    def move: ElasticMove = edge.move

    def assignBits(out: T): Unit =
      edge.assignBits(out)

    def driveMerge(canAccept: Bool, priorCandidate: Bool, allowed: Bool): Bool = {
      val candidate = edge.in.valid && edge.trigger

      edge.in.ready      := enable && !clear && edge.trigger && canAccept && allowed && !priorCandidate
      edge.move.fireWire := edge.in.valid && edge.in.ready

      candidate
    }
  }

  final private class StageIn(val edge: ElasticStageEdge[T, N]) extends IncomingRef {
    def order: Int        = edge.order
    def move: ElasticMove = edge.move

    def assignBits(out: T): Unit =
      edge.assignBits(out)

    def driveMerge(canAccept: Bool, priorCandidate: Bool, allowed: Bool): Bool = {
      val candidate = edge.wantWire

      edge.move.fireWire := edge.wantWire && canAccept && allowed && !priorCandidate

      candidate
    }
  }

  final private class RequestIn(val edge: ElasticRequestEdgeLike[T, N]) extends IncomingRef {
    def order: Int        = edge.order
    def move: ElasticMove = edge.move

    def assignBits(out: T): Unit =
      edge.assignBits(out)

    def driveMerge(canAccept: Bool, priorCandidate: Bool, allowed: Bool): Bool =
      edge.move.fire
  }

  final private class ResponseIn(val edge: ElasticResponseEdgeLike[T, N]) extends IncomingRef {
    def order: Int        = edge.order
    def move: ElasticMove = edge.move

    def assignBits(out: T): Unit =
      edge.assignBits(out)

    def driveMerge(canAccept: Bool, priorCandidate: Bool, allowed: Bool): Bool = {
      val candidate = edge.wantWire

      edge.driveMerge(canAccept && allowed, priorCandidate)

      candidate
    }
  }

  private val stages        = ArrayBuffer[ElasticStage[T, N]]()
  private val storages      = ArrayBuffer[StageStorage]()
  private val sourceEdges   = ArrayBuffer[ElasticSourceEdge[T, N]]()
  private val stageEdges    = ArrayBuffer[ElasticStageEdge[T, N]]()
  private val requestEdges  = ArrayBuffer[ElasticRequestEdgeLike[T, N]]()
  private val responseEdges = ArrayBuffer[ElasticResponseEdgeLike[T, N]]()
  private val sinkEdges     = ArrayBuffer[ElasticSinkEdgeLike[T, N]]()
  private val moves         = ArrayBuffer[ElasticMove]()
  private val reservedSlots = scala.collection.mutable.Map[Int, Int]()

  private var order = 0

  private def nextOrder(): Int = {
    val value = order
    order += 1
    value
  }

  private def boolOr(values: Iterable[Bool]): Bool =
    if (values.isEmpty) false.B else values.reduce(_ || _)

  private def keyOf(value: Data): BigInt =
    value.litOption.getOrElse(throw new NoSuchElementException(s"ElasticGraph node must be a literal enum value, got $value"))

  private def sameNode(lhs: Data, rhs: Data): Boolean =
    keyOf(lhs) == keyOf(rhs)

  private def nodeName(node: N): String =
    node.toString

  private def requireFreshNode(node: N): Unit =
    require(!stages.exists(stage => sameNode(stage.node, node)), s"duplicate elastic node: $node")

  private def outgoingOf(stage: ElasticStage[T, N]): Seq[OutgoingRef] = {
    val stageOut = stageEdges.filter(_.from.id == stage.id).map(edge => new StageOut(edge): OutgoingRef)
    val reqOut   = requestEdges.filter(_.from.id == stage.id).map(edge => new RequestOut(edge): OutgoingRef)
    val respOut  = responseEdges.filter(_.from.id == stage.id).map(edge => new ResponseOut(edge): OutgoingRef)
    val sinkOut  = sinkEdges.filter(_.from.id == stage.id).map(edge => new SinkOut(edge): OutgoingRef)

    (stageOut ++ reqOut ++ respOut ++ sinkOut).sortBy(_.order).toSeq
  }

  private def incomingOf(stage: ElasticStage[T, N]): Seq[IncomingRef] = {
    val sourceIn = sourceEdges.filter(_.to.id == stage.id).map(edge => new SourceIn(edge): IncomingRef)
    val stageIn  = stageEdges.filter(_.to.id == stage.id).map(edge => new StageIn(edge): IncomingRef)
    val reqIn    = requestEdges.filter(_.to.id == stage.id).map(edge => new RequestIn(edge): IncomingRef)
    val respIn   = responseEdges.filter(_.to.id == stage.id).map(edge => new ResponseIn(edge): IncomingRef)

    (sourceIn ++ stageIn ++ reqIn ++ respIn).sortBy(_.order).toSeq
  }

  private def reserveAllowed(stage: ElasticStage[T, N], incomingIndex: Int): Bool = {
    val slots = reservedSlots.getOrElse(stage.id, 0)

    if (slots == 0 || incomingIndex == 0) {
      true.B
    } else {
      stage.count < (stage.depth - slots).U
    }
  }

  private def makeStage(
    node: N,
    ready: Bool,
    depth: Int,
    isQueue: Boolean,
    isMerge: Boolean,
    pipe: Boolean,
    flow: Boolean,
    useSyncReadMem: Boolean
  ): ElasticStage[T, N] = {
    requireFreshNode(node)

    if (isQueue) {
      val q         = Module(new ElasticQueue(gen, depth, pipe = pipe, flow = flow, useSyncReadMem = useSyncReadMem))
      val readyWire = WireDefault(false.B)
      val fireWire  = WireDefault(false.B)

      val s = new ElasticStage[T, N](
        id = stages.length,
        node = node,
        depth = depth,
        isQueue = true,
        isMerge = isMerge,
        localReady = ready,
        validWire = q.io.deq.valid,
        bitsWire = q.io.deq.bits,
        readyWire = readyWire,
        fireWire = fireWire,
        countWire = q.io.count
      )

      stages += s
      storages += new StageStorage(s, None, None, Some(q))
      s
    } else {
      val valid     = RegInit(false.B)
      val bits      = Reg(gen)
      val readyWire = WireDefault(false.B)
      val fireWire  = WireDefault(false.B)
      val countWire = WireDefault(valid.asUInt)

      val s = new ElasticStage[T, N](
        id = stages.length,
        node = node,
        depth = 1,
        isQueue = false,
        isMerge = isMerge,
        localReady = ready,
        validWire = valid,
        bitsWire = bits,
        readyWire = readyWire,
        fireWire = fireWire,
        countWire = countWire
      )

      stages += s
      storages += new StageStorage(s, Some(valid), Some(bits), None)
      s
    }
  }

  def stage(node: N, ready: Bool = true.B): ElasticStage[T, N] =
    makeStage(node, ready, depth = 1, isQueue = false, isMerge = false, pipe = false, flow = false, useSyncReadMem = false)

  def queue(
    node: N,
    depth: Int,
    ready: Bool = true.B,
    pipe: Boolean = false,
    flow: Boolean = false,
    useSyncReadMem: Boolean = false
  ): ElasticStage[T, N] = {
    require(depth > 0, "queue depth must be > 0")
    makeStage(node, ready, depth = depth, isQueue = true, isMerge = false, pipe = pipe, flow = flow, useSyncReadMem = useSyncReadMem)
  }

  def merge(
    node: N,
    depth: Int = 1,
    ready: Bool = true.B,
    pipe: Boolean = false,
    flow: Boolean = false,
    useSyncReadMem: Boolean = false
  ): ElasticStage[T, N] = {
    require(depth > 0, "merge depth must be > 0")
    makeStage(node, ready, depth = depth, isQueue = true, isMerge = true, pipe = pipe, flow = flow, useSyncReadMem = useSyncReadMem)
  }

  def reserve(stage: ElasticStage[T, N], slots: Int): Unit = {
    require(stage.isMerge, s"reserve(...) is intended for merge(...) stages, got ${stage.node}")
    require(slots >= 0, "reserve slots must be >= 0")
    require(slots < stage.depth, s"reserve slots must be less than stage depth ${stage.depth}")

    reservedSlots(stage.id) = slots
  }

  def source(edge: (DecoupledIO[T], ElasticStage[T, N]), trigger: Bool = true.B): ElasticMove =
    sourceMap(edge, trigger)(_ => ())

  def sourceMap(edge: (DecoupledIO[T], ElasticStage[T, N]), trigger: Bool = true.B)(body: T => Unit): ElasticMove = {
    val in   = edge._1
    val to   = edge._2
    val fire = WireDefault(false.B)
    val move = new ElasticMove(s"source -> ${nodeName(to.node)}", fire)

    sourceEdges += new ElasticSourceEdge[T, N](
      in = in,
      to = to,
      trigger = trigger,
      mapFn = body,
      move = move,
      order = nextOrder()
    )

    moves += move
    move
  }

  def connect(edge: (ElasticStage[T, N], ElasticStage[T, N]), trigger: Bool = true.B): ElasticMove =
    connectMap(edge, trigger)(_ => ())

  def connectMap(edge: (ElasticStage[T, N], ElasticStage[T, N]), trigger: Bool = true.B)(body: T => Unit): ElasticMove = {
    val from = edge._1
    val to   = edge._2
    val fire = WireDefault(false.B)
    val want = WireDefault(false.B)
    val move = new ElasticMove(s"${nodeName(from.node)} -> ${nodeName(to.node)}", fire)

    stageEdges += new ElasticStageEdge[T, N](
      from = from,
      to = to,
      trigger = trigger,
      mapFn = body,
      move = move,
      wantWire = want,
      order = nextOrder()
    )

    moves += move
    move
  }

  def request[R <: Data](
    edge: ((ElasticStage[T, N], DecoupledIO[R]), ElasticStage[T, N]),
    trigger: Bool = true.B
  )(
    body: R => Unit
  ): ElasticMove =
    requestMap(edge, trigger)(body)(_ => ())

  def requestMap[R <: Data](
    edge: ((ElasticStage[T, N], DecoupledIO[R]), ElasticStage[T, N]),
    trigger: Bool = true.B
  )(
    reqBody: R => Unit
  )(
    waitBody: T => Unit
  ): ElasticMove = {
    val from = edge._1._1
    val req  = edge._1._2
    val wait = edge._2
    val fire = WireDefault(false.B)
    val move = new ElasticMove(s"${nodeName(from.node)} -> request -> ${nodeName(wait.node)}", fire)

    requestEdges += new ElasticRequestEdge[T, N, R](
      from = from,
      req = req,
      to = wait,
      trigger = trigger,
      reqMapFn = reqBody,
      waitMapFn = waitBody,
      move = move,
      order = nextOrder()
    )

    moves += move
    move
  }

  def response[R <: Data](
    edge: ((ElasticStage[T, N], DecoupledIO[R]), ElasticStage[T, N]),
    trigger: Bool = true.B
  )(
    body: (T, R) => Unit
  ): ElasticMove = {
    val wait = edge._1._1
    val resp = edge._1._2
    val to   = edge._2
    val fire = WireDefault(false.B)
    val want = WireDefault(false.B)
    val move = new ElasticMove(s"${nodeName(wait.node)} -> response -> ${nodeName(to.node)}", fire)

    responseEdges += new ElasticResponseEdge[T, N, R](
      from = wait,
      resp = resp,
      to = to,
      trigger = trigger,
      mapFn = body,
      move = move,
      wantWire = want,
      order = nextOrder()
    )

    moves += move
    move
  }

  def sink(edge: (ElasticStage[T, N], DecoupledIO[T]), trigger: Bool = true.B): ElasticMove =
    sinkMap(edge, trigger)(bits => bits := edge._1.bits)

  def sinkMap[R <: Data](edge: (ElasticStage[T, N], DecoupledIO[R]), trigger: Bool = true.B)(body: R => Unit): ElasticMove = {
    val from = edge._1
    val out  = edge._2
    val fire = WireDefault(false.B)
    val move = new ElasticMove(s"${nodeName(from.node)} -> sink", fire)

    sinkEdges += new ElasticSinkEdge[T, N, R](
      from = from,
      out = out,
      trigger = trigger,
      mapFn = body,
      move = move,
      order = nextOrder()
    )

    moves += move
    move
  }

  def route(from: ElasticStage[T, N])(build: ElasticRouteBuilder[T, N] => Unit): Unit = {
    val builder = new ElasticRouteBuilder[T, N](this, from)
    build(builder)
  }

  private[fsm] def finish(): ElasticGraph[T, N] = {
    require(stages.nonEmpty, "ElasticGraph requires at least one stage")

    for (edge <- stageEdges)
      require(
        edge.from.id < edge.to.id || edge.to.isMerge,
        s"ElasticGraph only supports forward stage edges unless the destination is merge(...), got ${edge.from.node} -> ${edge.to.node}"
      )

    for (edge <- requestEdges)
      require(!edge.to.isMerge, s"request(...) wait target should be queue(...) or stage(...), not merge(...): ${edge.to.node}")

    for (stage <- stages) {
      val incomingCount = sourceEdges.count(_.to.id == stage.id) + stageEdges.count(_.to.id == stage.id) + requestEdges.count(_.to.id == stage.id) + responseEdges.count(_.to.id == stage.id)
      require(stage.isMerge || incomingCount <= 1, s"ElasticGraph stage ${stage.node} has $incomingCount incoming edges; use merge(...) for multiple inputs")
    }

    val stageSeq        = stages.toSeq
    val storageSeq      = storages.toSeq
    val sourceEdgeSeq   = sourceEdges.toSeq
    val stageEdgeSeq    = stageEdges.toSeq
    val requestEdgeSeq  = requestEdges.toSeq
    val responseEdgeSeq = responseEdges.toSeq
    val sinkEdgeSeq     = sinkEdges.toSeq
    val moveSeq         = moves.toSeq

    val canAccept = Seq.fill(stageSeq.length)(WireDefault(false.B))
    val canLeave  = Seq.fill(stageSeq.length)(WireDefault(false.B))

    for (storage <- storageSeq)
      storage.queue.foreach { q =>
        q.io.clear     := clear
        q.io.enq.valid := false.B
        q.io.enq.bits  := 0.U.asTypeOf(gen)
        q.io.deq.ready := false.B
      }

    for (src <- sourceEdgeSeq) {
      src.in.ready      := false.B
      src.move.fireWire := false.B
    }

    for (edge <- stageEdgeSeq) {
      edge.wantWire      := false.B
      edge.move.fireWire := false.B
    }

    for (edge <- requestEdgeSeq)
      edge.init()

    for (edge <- responseEdgeSeq)
      edge.init()

    for (sink <- sinkEdgeSeq)
      sink.init()

    for (stage <- stageSeq.reverse) {
      var priorTrigger = false.B

      val leaveTerms = outgoingOf(stage).map { out =>
        val selected = out.trigger && !priorTrigger
        val leave    = out.leave(selected, canAccept)

        priorTrigger = priorTrigger || out.trigger
        leave
      }

      val storage = storageSeq(stage.id)

      canLeave(stage.id) := boolOr(leaveTerms)

      storage.queue match {
        case Some(q) =>
          canAccept(stage.id) := stage.localReady && q.io.enq.ready

        case None =>
          canAccept(stage.id) := stage.localReady && (!stage.valid || canLeave(stage.id))
      }

      stage.readyWire := canAccept(stage.id)
    }

    for (src <- sourceEdgeSeq if !src.to.isMerge) {
      src.in.ready      := enable && !clear && src.trigger && canAccept(src.to.id)
      src.move.fireWire := src.in.valid && src.in.ready
    }

    for (stage <- stageSeq) {
      var priorTrigger = false.B
      val active       = enable && !clear && stage.valid && stage.localReady

      val fireTerms = outgoingOf(stage).map { out =>
        val selected = out.trigger && !priorTrigger
        val fire     = out.drive(active, selected, canAccept)

        priorTrigger = priorTrigger || out.trigger
        fire
      }

      stage.fireWire := boolOr(fireTerms)
    }

    for (stage <- stageSeq if stage.isMerge) {
      var priorCandidate = false.B

      incomingOf(stage).zipWithIndex.foreach { case (in, index) =>
        val allowed   = reserveAllowed(stage, index)
        val candidate = in.driveMerge(canAccept(stage.id), priorCandidate, allowed)

        priorCandidate = priorCandidate || candidate
      }
    }

    for (stage <- stageSeq) {
      val storage  = storageSeq(stage.id)
      val incoming = incomingOf(stage)

      val incomingFire = boolOr(incoming.map(_.move.fire))
      val incomingBits = WireDefault(0.U.asTypeOf(gen))

      incoming.foreach { in =>
        when(in.move.fire) {
          in.assignBits(incomingBits)
        }
      }

      storage.queue match {
        case Some(q) =>
          q.io.enq.valid := incomingFire
          q.io.enq.bits  := incomingBits
          q.io.deq.ready := stage.fire

        case None =>
          val validReg = storage.validReg.get
          val bitsReg  = storage.bitsReg.get

          when(clear) {
            validReg := false.B
          }.elsewhen(enable) {
            when(incomingFire) {
              validReg := true.B
              bitsReg  := incomingBits
            }.elsewhen(stage.fire) {
              validReg := false.B
            }
          }
      }
    }

    new ElasticGraph[T, N](
      stages = stageSeq,
      moves = moveSeq
    )
  }
}

trait ElasticGraphSyntax { this: Module =>
  final protected def elastic[T <: Data, N <: Data](
    gen: T,
    nodeWitness: N,
    clear: Bool = false.B,
    enable: Bool = true.B
  )(
    build: ElasticGraphBuilder[T, N] => Unit
  ): ElasticGraph[T, N] = {
    val builder = new ElasticGraphBuilder[T, N](gen, clear, enable)
    build(builder)
    builder.finish()
  }
}
