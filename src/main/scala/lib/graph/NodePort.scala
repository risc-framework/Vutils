package vutils.graph

import chisel3._

trait NodeInterface extends Bundle

final case class NodePort[I <: Bundle, P <: Data](
  name: String,
  get: I => P
)

object NodeConnect {
  def connect[A <: Bundle, B <: Bundle, P <: Data](
    lhs: Node[A],
    lhsPort: NodePort[A, P],
    rhs: Node[B],
    rhsPort: NodePort[B, P]
  ): Unit = {
    lhsPort.get(lhs.io) <> rhsPort.get(rhs.io)
  }

  def expose[P <: Data](outer: P, inner: P): Unit = {
    outer <> inner
  }
}
