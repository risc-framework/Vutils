package vutils.fsm

import chisel3._

final class ElasticStage[T <: Data, N <: Data] private[fsm] (
  val id: Int,
  val node: N,
  val depth: Int,
  val isQueue: Boolean,
  val isMerge: Boolean,
  private[fsm] val localReady: Bool,
  private[fsm] val validWire: Bool,
  private[fsm] val bitsWire: T,
  private[fsm] val readyWire: Bool,
  private[fsm] val fireWire: Bool,
  private[fsm] val countWire: UInt
) {
  def valid: Bool = validWire
  def bits: T     = bitsWire
  def ready: Bool = readyWire
  def fire: Bool  = fireWire
  def count: UInt = countWire

  override def toString: String = s"ElasticStage($id, $node, depth=$depth, merge=$isMerge)"
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
  private def keyOf(value: Data): BigInt =
    value.litOption.getOrElse(throw new NoSuchElementException(s"ElasticGraph node must be a literal enum value, got $value"))

  def apply(index: Int): ElasticStage[T, N] = stages(index)

  def apply(node: Data): ElasticStage[T, N] =
    find(node).getOrElse(throw new NoSuchElementException(s"unknown elastic node: $node"))

  def find(node: Data): Option[ElasticStage[T, N]] = {
    val key = keyOf(node)
    stages.find(stage => keyOf(stage.node) == key)
  }
}
