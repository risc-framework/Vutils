package vutils.graph

import chisel3._

sealed trait NodeConnection[C] {
  private[vutils] def ports: Seq[NodePort[C, _ <: Data]]
  private[vutils] def connect(): Unit
}

final case class NodeEdge[C, PortData <: Data](
  source: NodeOutputPort[C, PortData],
  sink: NodeInputPort[C, PortData]
) extends NodeConnection[C] {
  require(
    source.protocol == sink.protocol,
    s"NodeEdge: cannot connect '${source.fullName}' to '${sink.fullName}': protocol mismatch ${source.protocol.name} != ${sink.protocol.name}"
  )

  override private[vutils] def ports: Seq[NodePort[C, _ <: Data]] =
    Seq(source, sink)

  override private[vutils] def connect(): Unit =
    sink.data <> source.data
}

final case class NodeExpose[C, PortData <: Data](
  left: NodePort[C, PortData],
  right: NodePort[C, PortData]
) extends NodeConnection[C] {
  require(
    left.role == right.role,
    s"NodeExpose: cannot expose '${left.fullName}' to '${right.fullName}': role mismatch ${left.role.name} != ${right.role.name}"
  )

  require(
    left.protocol == right.protocol,
    s"NodeExpose: cannot expose '${left.fullName}' to '${right.fullName}': protocol mismatch ${left.protocol.name} != ${right.protocol.name}"
  )

  override private[vutils] def ports: Seq[NodePort[C, _ <: Data]] =
    Seq(left, right)

  override private[vutils] def connect(): Unit =
    left.data <> right.data
}
