package vutils.graph

import chisel3._
import chisel3.util.{ Decoupled, DecoupledIO, Valid, ValidIO }
import scala.language.implicitConversions
import scala.reflect.ClassTag

sealed trait NodePortRole {
  def name: String
}

object NodePortRole {
  case object Source extends NodePortRole {
    override def name: String = "source"
  }

  case object Sink extends NodePortRole {
    override def name: String = "sink"
  }

  case object Raw extends NodePortRole {
    override def name: String = "raw"
  }
}

sealed trait NodePortProtocol {
  def name: String
}

object NodePortProtocol {
  case object Raw extends NodePortProtocol {
    override def name: String = "raw"
  }

  case object Valid extends NodePortProtocol {
    override def name: String = "valid"
  }

  case object Decoupled extends NodePortProtocol {
    override def name: String = "decoupled"
  }

  case object RawVec extends NodePortProtocol {
    override def name: String = "raw_vec"
  }

  case object ValidVec extends NodePortProtocol {
    override def name: String = "valid_vec"
  }

  case object DecoupledVec extends NodePortProtocol {
    override def name: String = "decoupled_vec"
  }

  def lane(protocol: NodePortProtocol): NodePortProtocol =
    protocol match {
      case RawVec       => Raw
      case ValidVec     => Valid
      case DecoupledVec => Decoupled
      case other        => other
    }
}

final class NodeDataLanes[Lane <: Data] private[vutils] (
  private[vutils] val vec: Vec[Lane]
) {
  def apply(idx: Int): Lane  = vec(idx)
  def apply(idx: UInt): Lane = vec(idx)
  def length: Int            = vec.length
  def raw: Vec[Lane]         = vec
}

object NodeDataLanes {
  implicit def toVec[Lane <: Data](lanes: NodeDataLanes[Lane]): Vec[Lane] =
    lanes.vec
}

final class NodeRawLanes[Lane <: Data] private[vutils] (
  private[vutils] val vec: Vec[Lane]
) {
  val lanes: NodeDataLanes[Lane] = new NodeDataLanes(vec)

  def apply(idx: Int): Lane  = vec(idx)
  def apply(idx: UInt): Lane = vec(idx)
  def length: Int            = vec.length
  def raw: Vec[Lane]         = vec
}

object NodeRawLanes {
  implicit def toVec[Lane <: Data](lanes: NodeRawLanes[Lane]): Vec[Lane] =
    lanes.vec
}

final class NodeInLanes[C, Lane <: Data] private[vutils] (
  owner: Node[C],
  name: String,
  protocol: NodePortProtocol,
  fullName: String,
  private[vutils] val vec: Vec[Lane]
) {
  def apply(idx: Int): NodeInLane[C, Lane] = {
    require(idx >= 0 && idx < vec.length, s"NodeInVec '$fullName' lane index $idx out of range 0..${vec.length - 1}")
    new NodeInLane(owner, s"${name}_$idx", NodePortProtocol.lane(protocol), idx, vec(idx))
  }

  def apply(idx: UInt): Lane = vec(idx)
  def length: Int            = vec.length
  def raw: Vec[Lane]         = vec
}

object NodeInLanes {
  implicit def toVec[C, Lane <: Data](lanes: NodeInLanes[C, Lane]): Vec[Lane] =
    lanes.vec
}

final class NodeOutLanes[C, Lane <: Data] private[vutils] (
  owner: Node[C],
  name: String,
  protocol: NodePortProtocol,
  fullName: String,
  private[vutils] val vec: Vec[Lane]
) {
  def apply(idx: Int): NodeOutLane[C, Lane] = {
    require(idx >= 0 && idx < vec.length, s"NodeOutVec '$fullName' lane index $idx out of range 0..${vec.length - 1}")
    new NodeOutLane(owner, s"${name}_$idx", NodePortProtocol.lane(protocol), idx, vec(idx))
  }

  def apply(idx: UInt): Lane = vec(idx)
  def length: Int            = vec.length
  def raw: Vec[Lane]         = vec
}

object NodeOutLanes {
  implicit def toVec[C, Lane <: Data](lanes: NodeOutLanes[C, Lane]): Vec[Lane] =
    lanes.vec
}

sealed trait NodePort[C, PortData <: Data] {
  def owner: Node[C]
  def name: String
  def role: NodePortRole
  def protocol: NodePortProtocol

  private[vutils] def data: PortData

  final def fullName: String =
    s"${owner.nodeName}.$name"
}

sealed trait NodeInputPort[C, PortData <: Data] extends NodePort[C, PortData] {
  final def ->(peer: NodeInputPort[C, PortData]): NodeExpose[C, PortData] =
    NodeExpose(this, peer)
}

sealed trait NodeOutputPort[C, PortData <: Data] extends NodePort[C, PortData] {
  final def ->(sink: NodeInputPort[C, PortData]): NodeEdge[C, PortData] =
    NodeEdge(this, sink)

  final def ->(peer: NodeOutputPort[C, PortData]): NodeExpose[C, PortData] =
    NodeExpose(this, peer)
}

sealed trait NodeRawPort[C, PortData <: Data] extends NodePort[C, PortData] {
  final def ->(peer: NodeRawPort[C, PortData]): NodeRawEdge[C, PortData] =
    NodeRawEdge(this, peer)
}

final class NodeRaw[C, PortData <: Data] private[vutils] (
  val owner: Node[C],
  val name: String,
  val protocol: NodePortProtocol,
  val io: PortData
) extends NodeRawPort[C, PortData] {
  override val role: NodePortRole             = NodePortRole.Raw
  override private[vutils] val data: PortData = io

  io.suggestName(name)
}

final class NodeIn[C, PortData <: Data] private[vutils] (
  val owner: Node[C],
  val name: String,
  val protocol: NodePortProtocol,
  val in: PortData
) extends NodeInputPort[C, PortData] {
  override val role: NodePortRole             = NodePortRole.Sink
  override private[vutils] val data: PortData = in

  in.suggestName(name)
}

final class NodeOut[C, PortData <: Data] private[vutils] (
  val owner: Node[C],
  val name: String,
  val protocol: NodePortProtocol,
  val out: PortData
) extends NodeOutputPort[C, PortData] {
  override val role: NodePortRole             = NodePortRole.Source
  override private[vutils] val data: PortData = out

  out.suggestName(name)
}

final class NodeInLane[C, Lane <: Data] private[vutils] (
  val owner: Node[C],
  val name: String,
  val protocol: NodePortProtocol,
  val index: Int,
  val in: Lane
) extends NodeInputPort[C, Lane] {
  override val role: NodePortRole         = NodePortRole.Sink
  override private[vutils] val data: Lane = in
}

final class NodeOutLane[C, Lane <: Data] private[vutils] (
  val owner: Node[C],
  val name: String,
  val protocol: NodePortProtocol,
  val index: Int,
  val out: Lane
) extends NodeOutputPort[C, Lane] {
  override val role: NodePortRole         = NodePortRole.Source
  override private[vutils] val data: Lane = out
}

final class NodeInVec[C, Lane <: Data] private[vutils] (
  val owner: Node[C],
  val name: String,
  val protocol: NodePortProtocol,
  raw: Vec[Lane]
) extends NodeInputPort[C, Vec[Lane]] {
  override val role: NodePortRole              = NodePortRole.Sink
  override private[vutils] val data: Vec[Lane] = raw

  val in: NodeRawLanes[Lane]      = new NodeRawLanes(raw)
  val lanes: NodeInLanes[C, Lane] = new NodeInLanes(owner, name, protocol, fullName, raw)

  def vec: Vec[Lane]                       = raw
  def lane(idx: Int): NodeInLane[C, Lane]  = lanes(idx)
  def apply(idx: Int): NodeInLane[C, Lane] = lanes(idx)
  def apply(idx: UInt): Lane               = raw(idx)

  raw.suggestName(name)
}

final class NodeOutVec[C, Lane <: Data] private[vutils] (
  val owner: Node[C],
  val name: String,
  val protocol: NodePortProtocol,
  raw: Vec[Lane]
) extends NodeOutputPort[C, Vec[Lane]] {
  override val role: NodePortRole              = NodePortRole.Source
  override private[vutils] val data: Vec[Lane] = raw

  val out: NodeRawLanes[Lane]      = new NodeRawLanes(raw)
  val lanes: NodeOutLanes[C, Lane] = new NodeOutLanes(owner, name, protocol, fullName, raw)

  def vec: Vec[Lane]                        = raw
  def lane(idx: Int): NodeOutLane[C, Lane]  = lanes(idx)
  def apply(idx: Int): NodeOutLane[C, Lane] = lanes(idx)
  def apply(idx: UInt): Lane                = raw(idx)

  raw.suggestName(name)
}

object NodePort {
  def raw[C, P <: Data: ClassTag](owner: Node[C], name: String)(implicit ctx: C): NodeRaw[C, P] =
    rawWith(owner, name)(ctx => PayloadFactory.auto[C, P](ctx))

  def rawWith[C, P <: Data](owner: Node[C], name: String)(payload: C => P)(implicit ctx: C): NodeRaw[C, P] =
    new NodeRaw(owner, name, NodePortProtocol.Raw, IO(payload(ctx)))

  def input[C, P <: Data: ClassTag](owner: Node[C], name: String)(implicit ctx: C): NodeIn[C, P] =
    inputWith(owner, name)(ctx => PayloadFactory.auto[C, P](ctx))

  def output[C, P <: Data: ClassTag](owner: Node[C], name: String)(implicit ctx: C): NodeOut[C, P] =
    outputWith(owner, name)(ctx => PayloadFactory.auto[C, P](ctx))

  def inputWith[C, P <: Data](owner: Node[C], name: String)(payload: C => P)(implicit ctx: C): NodeIn[C, P] =
    new NodeIn(owner, name, NodePortProtocol.Raw, IO(Input(payload(ctx))))

  def outputWith[C, P <: Data](owner: Node[C], name: String)(payload: C => P)(implicit ctx: C): NodeOut[C, P] =
    new NodeOut(owner, name, NodePortProtocol.Raw, IO(Output(payload(ctx))))

  def validInput[C, P <: Data: ClassTag](owner: Node[C], name: String)(implicit ctx: C): NodeIn[C, ValidIO[P]] =
    validInputWith(owner, name)(ctx => PayloadFactory.auto[C, P](ctx))

  def validOutput[C, P <: Data: ClassTag](owner: Node[C], name: String)(implicit ctx: C): NodeOut[C, ValidIO[P]] =
    validOutputWith(owner, name)(ctx => PayloadFactory.auto[C, P](ctx))

  def validInputWith[C, P <: Data](owner: Node[C], name: String)(payload: C => P)(implicit ctx: C): NodeIn[C, ValidIO[P]] =
    new NodeIn(owner, name, NodePortProtocol.Valid, IO(Flipped(Valid(payload(ctx)))))

  def validOutputWith[C, P <: Data](owner: Node[C], name: String)(payload: C => P)(implicit ctx: C): NodeOut[C, ValidIO[P]] =
    new NodeOut(owner, name, NodePortProtocol.Valid, IO(Valid(payload(ctx))))

  def decoupledInput[C, P <: Data: ClassTag](owner: Node[C], name: String)(implicit ctx: C): NodeIn[C, DecoupledIO[P]] =
    decoupledInputWith(owner, name)(ctx => PayloadFactory.auto[C, P](ctx))

  def decoupledOutput[C, P <: Data: ClassTag](owner: Node[C], name: String)(implicit ctx: C): NodeOut[C, DecoupledIO[P]] =
    decoupledOutputWith(owner, name)(ctx => PayloadFactory.auto[C, P](ctx))

  def decoupledInputWith[C, P <: Data](owner: Node[C], name: String)(payload: C => P)(implicit ctx: C): NodeIn[C, DecoupledIO[P]] =
    new NodeIn(owner, name, NodePortProtocol.Decoupled, IO(Flipped(Decoupled(payload(ctx)))))

  def decoupledOutputWith[C, P <: Data](owner: Node[C], name: String)(payload: C => P)(implicit ctx: C): NodeOut[C, DecoupledIO[P]] =
    new NodeOut(owner, name, NodePortProtocol.Decoupled, IO(Decoupled(payload(ctx))))

  def vecInput[C, P <: Data: ClassTag](owner: Node[C], name: String, lanes: C => Int)(implicit ctx: C): NodeInVec[C, P] =
    vecInputWith(owner, name, lanes)(ctx => PayloadFactory.auto[C, P](ctx))

  def vecOutput[C, P <: Data: ClassTag](owner: Node[C], name: String, lanes: C => Int)(implicit ctx: C): NodeOutVec[C, P] =
    vecOutputWith(owner, name, lanes)(ctx => PayloadFactory.auto[C, P](ctx))

  def vecInputWith[C, P <: Data](owner: Node[C], name: String, lanes: C => Int)(payload: C => P)(implicit ctx: C): NodeInVec[C, P] =
    new NodeInVec(owner, name, NodePortProtocol.RawVec, IO(Input(Vec(lanes(ctx), payload(ctx)))))

  def vecOutputWith[C, P <: Data](owner: Node[C], name: String, lanes: C => Int)(payload: C => P)(implicit ctx: C): NodeOutVec[C, P] =
    new NodeOutVec(owner, name, NodePortProtocol.RawVec, IO(Output(Vec(lanes(ctx), payload(ctx)))))

  def validVecInput[C, P <: Data: ClassTag](owner: Node[C], name: String, lanes: C => Int)(implicit ctx: C): NodeInVec[C, ValidIO[P]] =
    validVecInputWith(owner, name, lanes)(ctx => PayloadFactory.auto[C, P](ctx))

  def validVecOutput[C, P <: Data: ClassTag](owner: Node[C], name: String, lanes: C => Int)(implicit ctx: C): NodeOutVec[C, ValidIO[P]] =
    validVecOutputWith(owner, name, lanes)(ctx => PayloadFactory.auto[C, P](ctx))

  def validVecInputWith[C, P <: Data](owner: Node[C], name: String, lanes: C => Int)(payload: C => P)(implicit ctx: C): NodeInVec[C, ValidIO[P]] =
    new NodeInVec(owner, name, NodePortProtocol.ValidVec, IO(Flipped(Vec(lanes(ctx), Valid(payload(ctx))))))

  def validVecOutputWith[C, P <: Data](owner: Node[C], name: String, lanes: C => Int)(payload: C => P)(implicit ctx: C): NodeOutVec[C, ValidIO[P]] =
    new NodeOutVec(owner, name, NodePortProtocol.ValidVec, IO(Vec(lanes(ctx), Valid(payload(ctx)))))

  def decoupledVecInput[C, P <: Data: ClassTag](owner: Node[C], name: String, lanes: C => Int)(implicit ctx: C): NodeInVec[C, DecoupledIO[P]] =
    decoupledVecInputWith(owner, name, lanes)(ctx => PayloadFactory.auto[C, P](ctx))

  def decoupledVecOutput[C, P <: Data: ClassTag](owner: Node[C], name: String, lanes: C => Int)(implicit ctx: C): NodeOutVec[C, DecoupledIO[P]] =
    decoupledVecOutputWith(owner, name, lanes)(ctx => PayloadFactory.auto[C, P](ctx))

  def decoupledVecInputWith[C, P <: Data](owner: Node[C], name: String, lanes: C => Int)(payload: C => P)(implicit ctx: C): NodeInVec[C, DecoupledIO[P]] =
    new NodeInVec(owner, name, NodePortProtocol.DecoupledVec, IO(Flipped(Vec(lanes(ctx), Decoupled(payload(ctx))))))

  def decoupledVecOutputWith[C, P <: Data](owner: Node[C], name: String, lanes: C => Int)(payload: C => P)(implicit ctx: C): NodeOutVec[C, DecoupledIO[P]] =
    new NodeOutVec(owner, name, NodePortProtocol.DecoupledVec, IO(Vec(lanes(ctx), Decoupled(payload(ctx)))))
}
