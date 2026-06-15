package vutils.fsm

import chisel3._
import chisel3.util.MuxCase
import scala.collection.mutable.ArrayBuffer

final class MooreNode[S <: Data] private[fsm] (
  val id: Int,
  val value: S,
  private[fsm] val activeWire: Bool,
  private[fsm] val nextWire: Bool
) {
  def active: Bool = activeWire
  def next: Bool   = nextWire

  override def toString: String = s"MooreNode($id, $value)"
}

final class MooreEvent[S <: Data] private[fsm] (
  val from: MooreNode[S],
  val to: MooreNode[S],
  val trigger: Bool,
  private var bodyFn: () => Unit,
  private[fsm] val fireWire: Bool,
  val order: Int
) {
  def fire: Bool                                   = fireWire
  private[fsm] def setBody(body: () => Unit): Unit = bodyFn = body
  private[fsm] def body(): Unit                    = bodyFn()

  override def toString: String = s"MooreEvent(${from.value} -> ${to.value})"
}

final class MooreEventHandle[S <: Data] private[fsm] (
  private val event: MooreEvent[S]
) {
  def apply(body: => Unit): MooreEvent[S] = {
    event.setBody(() => body)
    event
  }

  def fire: Bool           = event.fire
  def value: MooreEvent[S] = event
}

final class MooreGraph[S <: Data] private[fsm] (
  val state: S,
  val nextState: S,
  val nodes: Seq[MooreNode[S]],
  val events: Seq[MooreEvent[S]]
) {
  private def keyOf(value: Data): BigInt =
    value.litOption.getOrElse(throw new NoSuchElementException(s"MooreGraph node must be a literal enum value, got $value"))

  def apply(index: Int): MooreNode[S] = nodes(index)

  def apply(value: Data): MooreNode[S] = {
    val key = keyOf(value)
    nodes.find(node => keyOf(node.value) == key).getOrElse(throw new NoSuchElementException(s"unknown Moore node: $value"))
  }

  def event(index: Int): MooreEvent[S] = events(index)
}

final class MooreBuilder[S <: Data] private[fsm] (
  start: S,
  clear: Bool,
  enable: Bool
) {
  private val nodes  = ArrayBuffer[MooreNode[S]]()
  private val events = ArrayBuffer[MooreEvent[S]]()
  private var order  = 0

  private type Edge = (MooreNode[S], MooreNode[S])

  private def nextOrder(): Int = {
    val value = order
    order += 1
    value
  }

  private def keyOf(value: Data): BigInt =
    value.litOption.getOrElse(throw new NoSuchElementException(s"Moore state must be a literal enum value, got $value"))

  def state(value: S): MooreNode[S] = {
    val key = keyOf(value)
    require(!nodes.exists(node => keyOf(node.value) == key), s"duplicate Moore state: $value")

    val active = WireDefault(false.B)
    val next   = WireDefault(false.B)

    val node = new MooreNode[S](
      id = nodes.length,
      value = value,
      activeWire = active,
      nextWire = next
    )

    nodes += node
    node
  }

  private def addEvent(from: MooreNode[S], to: MooreNode[S], trigger: Bool): MooreEventHandle[S] = {
    val fire = WireDefault(false.B)

    val event = new MooreEvent[S](
      from = from,
      to = to,
      trigger = trigger,
      bodyFn = () => (),
      fireWire = fire,
      order = nextOrder()
    )

    events += event
    new MooreEventHandle(event)
  }

  def trans(edge: Edge): MooreEventHandle[S] =
    addEvent(edge._1, edge._2, true.B)

  def trans(edge: Edge, trigger: Bool): MooreEventHandle[S] =
    addEvent(edge._1, edge._2, trigger)

  def trans(from: MooreNode[S], to: MooreNode[S]): MooreEventHandle[S] =
    trans(from -> to)

  def trans(from: MooreNode[S], to: MooreNode[S], trigger: Bool): MooreEventHandle[S] =
    trans(from -> to, trigger)

  def action(at: MooreNode[S], trigger: Bool = true.B): MooreEventHandle[S] =
    addEvent(at, at, trigger)

  private[fsm] def finish(): MooreGraph[S] = {
    require(nodes.nonEmpty, "Moore requires at least one state")

    val startKey = keyOf(start)
    require(nodes.exists(node => keyOf(node.value) == startKey), s"Moore start state $start is not declared by state(...)")

    val nodeSeq  = nodes.toSeq
    val eventSeq = events.toSeq

    val stateReg = RegInit(start)

    val hits      = eventSeq.map(event => enable && !clear && event.trigger && stateReg === event.from.value)
    val priorHits = hits.scanLeft(false.B)(_ || _).dropRight(1)
    val fires     = hits.zip(priorHits).map { case (hit, prior) => hit && !prior }

    val nextPairs: Seq[(Bool, S)] = eventSeq.zip(fires).map { case (event, fire) => fire -> event.to.value }
    val next                      = MuxCase(stateReg, nextPairs)

    when(clear) {
      stateReg := start
    }.elsewhen(enable) {
      stateReg := next
    }

    nodeSeq.foreach { node =>
      node.activeWire := stateReg === node.value
      node.nextWire   := next === node.value
    }

    eventSeq.zip(fires).foreach { case (event, fire) =>
      event.fireWire := fire
      when(fire) {
        event.body()
      }
    }

    new MooreGraph[S](
      state = stateReg,
      nextState = next,
      nodes = nodeSeq,
      events = eventSeq
    )
  }
}

trait Moore { this: Module =>
  final protected def moore[S <: Data](
    start: S,
    clear: Bool = false.B,
    enable: Bool = true.B
  )(
    build: MooreBuilder[S] => Unit
  ): MooreGraph[S] = {
    val builder = new MooreBuilder[S](start, clear, enable)
    build(builder)
    builder.finish()
  }
}
