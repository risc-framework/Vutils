package vutils.graph

import chisel3._

final case class NodeType(name: String) {
  override def toString: String = name
}

abstract class Node[I <: Bundle](gen: => I) extends Module {
  def nodeType: NodeType

  val io: I = IO(gen)
}

trait NodeImpl[I <: Bundle] extends NodeNamed {
  def nodeType: NodeType
  def selector: NodeSelector
  def build: Node[I]

  def matches(required: NodeSelector): Boolean =
    selector.matches(required)

  def score(required: NodeSelector): Int =
    selector.score(required)
}

class NodeImplRegistry[I <: Bundle](val nodeType: NodeType) extends NodeRegistry[NodeImpl[I]](s"Node:${nodeType.name}") {
  def select(required: NodeSelector): NodeImpl[I] = {
    val candidates = getAll().filter(impl => impl.nodeType == nodeType && impl.matches(required))
    val bestScore  = candidates.map(_.score(required)).reduceOption(_ max _).getOrElse(-1)
    val best       = candidates.filter(_.score(required) == bestScore)

    if (best.isEmpty) {
      throw new NoSuchElementException(s"Node '${nodeType.name}': no implementation for selector '${required.canonicalName}'. Available: ${getAll().map(_.name).mkString(", ")}")
    }

    if (best.size > 1) {
      throw new IllegalArgumentException(s"Node '${nodeType.name}': ambiguous selector '${required.canonicalName}'. Candidates: ${best.map(_.name).mkString(", ")}")
    }

    best.head
  }

  def build(config: NodeConfig): Node[I] =
    Module(select(config.selector).build)

  def build(selector: NodeSelector): Node[I] =
    Module(select(selector).build)
}
