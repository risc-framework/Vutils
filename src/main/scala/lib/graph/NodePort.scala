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

private[vutils] object NodePortDefault {
  def localSource(protocol: NodePortProtocol, data: Data): Unit =
    protocol match {
      case NodePortProtocol.Raw =>
        data := 0.U.asTypeOf(data)

      case NodePortProtocol.Valid =>
        val v = data.asInstanceOf[ValidIO[Data]]
        v.valid := false.B
        v.bits  := DontCare

      case NodePortProtocol.Decoupled =>
        val d = data.asInstanceOf[DecoupledIO[Data]]
        d.valid := false.B
        d.bits  := DontCare

      case NodePortProtocol.RawVec =>
        data := 0.U.asTypeOf(data)

      case NodePortProtocol.ValidVec =>
        val v = data.asInstanceOf[Vec[ValidIO[Data]]]
        for (i <- 0 until v.length) {
          v(i).valid := false.B
          v(i).bits  := DontCare
        }

      case NodePortProtocol.DecoupledVec =>
        val d = data.asInstanceOf[Vec[DecoupledIO[Data]]]
        for (i <- 0 until d.length) {
          d(i).valid := false.B
          d(i).bits  := DontCare
        }
    }

  def localSink(protocol: NodePortProtocol, data: Data): Unit =
    protocol match {
      case NodePortProtocol.Decoupled =>
        val d = data.asInstanceOf[DecoupledIO[Data]]
        d.ready := false.B

      case NodePortProtocol.DecoupledVec =>
        val d = data.asInstanceOf[Vec[DecoupledIO[Data]]]
        for (i <- 0 until d.length)
          d(i).ready := false.B

      case _ =>
    }

  def parentDriveSink(protocol: NodePortProtocol, data: Data): Unit =
    protocol match {
      case NodePortProtocol.Raw =>
        data := 0.U.asTypeOf(data)

      case NodePortProtocol.Valid =>
        val v = data.asInstanceOf[ValidIO[Data]]
        v.valid := false.B
        v.bits  := DontCare

      case NodePortProtocol.Decoupled =>
        val d = data.asInstanceOf[DecoupledIO[Data]]
        d.valid := false.B
        d.bits  := DontCare

      case NodePortProtocol.RawVec =>
        data := 0.U.asTypeOf(data)

      case NodePortProtocol.ValidVec =>
        val v = data.asInstanceOf[Vec[ValidIO[Data]]]
        for (i <- 0 until v.length) {
          v(i).valid := false.B
          v(i).bits  := DontCare
        }

      case NodePortProtocol.DecoupledVec =>
        val d = data.asInstanceOf[Vec[DecoupledIO[Data]]]
        for (i <- 0 until d.length) {
          d(i).valid := false.B
          d(i).bits  := DontCare
        }
    }

  def parentDriveSource(protocol: NodePortProtocol, data: Data): Unit =
    protocol match {
      case NodePortProtocol.Decoupled =>
        val d = data.asInstanceOf[DecoupledIO[Data]]
        d.ready := false.B

      case NodePortProtocol.DecoupledVec =>
        val d = data.asInstanceOf[Vec[DecoupledIO[Data]]]
        for (i <- 0 until d.length)
          d(i).ready := false.B

      case _ =>
    }
}

final class NodeDataLanes[Lane <: Data] private[vutils] (
  private[this] val rawVec: Vec[Lane]
) {
  def apply(idx: Int): Lane  = rawVec(idx)
  def apply(idx: UInt): Lane = rawVec(idx)
  def length: Int            = rawVec.length
  def raw: Vec[Lane]         = rawVec
}

object NodeDataLanes {
  implicit def toVec[Lane <: Data](lanes: NodeDataLanes[Lane]): Vec[Lane] =
    lanes.raw
}

final class NodeRawLanes[Lane <: Data] private[vutils] (
  private[this] val rawVec: Vec[Lane]
) {
  val lanes: NodeDataLanes[Lane] = new NodeDataLanes(rawVec)

  def apply(idx: Int): Lane  = rawVec(idx)
  def apply(idx: UInt): Lane = rawVec(idx)
  def length: Int            = rawVec.length
  def raw: Vec[Lane]         = rawVec
}

object NodeRawLanes {
  implicit def toVec[Lane <: Data](lanes: NodeRawLanes[Lane]): Vec[Lane] =
    lanes.raw
}

final class NodeInLanes[C, Lane <: Data] private[vutils] (
  owner: Node[C],
  name: String,
  protocol: NodePortProtocol,
  fullName: String,
  required: Boolean,
  private[this] val rawVec: Vec[Lane]
) {
  def apply(idx: Int): NodeInLane[C, Lane] = {
    require(idx >= 0 && idx < rawVec.length, s"NodeInVec '$fullName' lane index $idx out of range 0..${rawVec.length - 1}")
    new NodeInLane(owner, s"${name}_$idx", NodePortProtocol.lane(protocol), required, idx, rawVec(idx), parentName = Some(name))
  }

  def apply(idx: UInt): Lane = rawVec(idx)
  def length: Int            = rawVec.length
  def raw: Vec[Lane]         = rawVec
}

object NodeInLanes {
  implicit def toVec[C, Lane <: Data](lanes: NodeInLanes[C, Lane]): Vec[Lane] =
    lanes.raw
}

final class NodeOutLanes[C, Lane <: Data] private[vutils] (
  owner: Node[C],
  name: String,
  protocol: NodePortProtocol,
  fullName: String,
  required: Boolean,
  private[this] val rawVec: Vec[Lane]
) {
  def apply(idx: Int): NodeOutLane[C, Lane] = {
    require(idx >= 0 && idx < rawVec.length, s"NodeOutVec '$fullName' lane index $idx out of range 0..${rawVec.length - 1}")
    new NodeOutLane(owner, s"${name}_$idx", NodePortProtocol.lane(protocol), required, idx, rawVec(idx), parentName = Some(name))
  }

  def apply(idx: UInt): Lane = rawVec(idx)
  def length: Int            = rawVec.length
  def raw: Vec[Lane]         = rawVec
}

object NodeOutLanes {
  implicit def toVec[C, Lane <: Data](lanes: NodeOutLanes[C, Lane]): Vec[Lane] =
    lanes.raw
}

sealed trait NodePort[C, PortData <: Data] {
  def owner: Node[C]
  def name: String
  def role: NodePortRole
  def protocol: NodePortProtocol
  def required: Boolean

  private[vutils] def data: PortData
  private[vutils] def parentName: Option[String] = None

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
  val required: Boolean,
  private[this] val portData: PortData
) extends NodeRawPort[C, PortData] {
  override val role: NodePortRole             = NodePortRole.Raw
  override private[vutils] def data: PortData = portData
  def io: PortData                            = portData
}

final class NodeIn[C, PortData <: Data] private[vutils] (
  val owner: Node[C],
  val name: String,
  val protocol: NodePortProtocol,
  val required: Boolean,
  private[this] val portData: PortData
) extends NodeInputPort[C, PortData] {
  override val role: NodePortRole             = NodePortRole.Sink
  override private[vutils] def data: PortData = portData
  def in: PortData                            = portData

  if (!required) {
    NodePortDefault.localSink(protocol, portData)
  }
}

final class NodeOut[C, PortData <: Data] private[vutils] (
  val owner: Node[C],
  val name: String,
  val protocol: NodePortProtocol,
  val required: Boolean,
  private[this] val portData: PortData
) extends NodeOutputPort[C, PortData] {
  override val role: NodePortRole             = NodePortRole.Source
  override private[vutils] def data: PortData = portData
  def out: PortData                           = portData

  if (!required) {
    NodePortDefault.localSource(protocol, portData)
  }
}

final class NodeInLane[C, Lane <: Data] private[vutils] (
  val owner: Node[C],
  val name: String,
  val protocol: NodePortProtocol,
  val required: Boolean,
  val index: Int,
  private[this] val portData: Lane,
  override private[vutils] val parentName: Option[String]
) extends NodeInputPort[C, Lane] {
  override val role: NodePortRole         = NodePortRole.Sink
  override private[vutils] def data: Lane = portData
  def in: Lane                            = portData
}

final class NodeOutLane[C, Lane <: Data] private[vutils] (
  val owner: Node[C],
  val name: String,
  val protocol: NodePortProtocol,
  val required: Boolean,
  val index: Int,
  private[this] val portData: Lane,
  override private[vutils] val parentName: Option[String]
) extends NodeOutputPort[C, Lane] {
  override val role: NodePortRole         = NodePortRole.Source
  override private[vutils] def data: Lane = portData
  def out: Lane                           = portData
}

final class NodeInVec[C, Lane <: Data] private[vutils] (
  val owner: Node[C],
  val name: String,
  val protocol: NodePortProtocol,
  val required: Boolean,
  private[this] val rawVec: Vec[Lane]
) extends NodeInputPort[C, Vec[Lane]] {
  override val role: NodePortRole              = NodePortRole.Sink
  override private[vutils] def data: Vec[Lane] = rawVec

  val in: NodeRawLanes[Lane]      = new NodeRawLanes(rawVec)
  val lanes: NodeInLanes[C, Lane] = new NodeInLanes(owner, name, protocol, fullName, required, rawVec)

  def vec: Vec[Lane]                       = rawVec
  def raw: Vec[Lane]                       = rawVec
  def lane(idx: Int): NodeInLane[C, Lane]  = lanes(idx)
  def apply(idx: Int): NodeInLane[C, Lane] = lanes(idx)
  def apply(idx: UInt): Lane               = rawVec(idx)

  if (!required) {
    NodePortDefault.localSink(protocol, rawVec)
  }
}

final class NodeOutVec[C, Lane <: Data] private[vutils] (
  val owner: Node[C],
  val name: String,
  val protocol: NodePortProtocol,
  val required: Boolean,
  private[this] val rawVec: Vec[Lane]
) extends NodeOutputPort[C, Vec[Lane]] {
  override val role: NodePortRole              = NodePortRole.Source
  override private[vutils] def data: Vec[Lane] = rawVec

  val out: NodeRawLanes[Lane]      = new NodeRawLanes(rawVec)
  val lanes: NodeOutLanes[C, Lane] = new NodeOutLanes(owner, name, protocol, fullName, required, rawVec)

  def vec: Vec[Lane]                        = rawVec
  def raw: Vec[Lane]                        = rawVec
  def lane(idx: Int): NodeOutLane[C, Lane]  = lanes(idx)
  def apply(idx: Int): NodeOutLane[C, Lane] = lanes(idx)
  def apply(idx: UInt): Lane                = rawVec(idx)

  if (!required) {
    NodePortDefault.localSource(protocol, rawVec)
  }
}

object NodePort {
  private def namedIO[P <: Data](name: String)(gen: => P): P = {
    val result = IO(gen)
    result.suggestName(name)
    result
  }

  def raw[C, P <: Data: ClassTag](owner: Node[C], name: String, required: Boolean = false)(implicit ctx: C): NodeRaw[C, P] =
    rawWith(owner, name, required)(ctx => PayloadFactory.auto[C, P](ctx))

  def rawWith[C, P <: Data](owner: Node[C], name: String, required: Boolean = false)(payload: C => P)(implicit ctx: C): NodeRaw[C, P] =
    new NodeRaw(owner, name, NodePortProtocol.Raw, required, namedIO(name)(payload(ctx)))

  def input[C, P <: Data: ClassTag](owner: Node[C], name: String, required: Boolean = false)(implicit ctx: C): NodeIn[C, P] =
    inputWith(owner, name, required)(ctx => PayloadFactory.auto[C, P](ctx))

  def output[C, P <: Data: ClassTag](owner: Node[C], name: String, required: Boolean = false)(implicit ctx: C): NodeOut[C, P] =
    outputWith(owner, name, required)(ctx => PayloadFactory.auto[C, P](ctx))

  def inputWith[C, P <: Data](owner: Node[C], name: String, required: Boolean = false)(payload: C => P)(implicit ctx: C): NodeIn[C, P] =
    new NodeIn(owner, name, NodePortProtocol.Raw, required, namedIO(name)(Input(payload(ctx))))

  def outputWith[C, P <: Data](owner: Node[C], name: String, required: Boolean = false)(payload: C => P)(implicit ctx: C): NodeOut[C, P] =
    new NodeOut(owner, name, NodePortProtocol.Raw, required, namedIO(name)(Output(payload(ctx))))

  def validInput[C, P <: Data: ClassTag](owner: Node[C], name: String, required: Boolean = false)(implicit ctx: C): NodeIn[C, ValidIO[P]] =
    validInputWith(owner, name, required)(ctx => PayloadFactory.auto[C, P](ctx))

  def validOutput[C, P <: Data: ClassTag](owner: Node[C], name: String, required: Boolean = false)(implicit ctx: C): NodeOut[C, ValidIO[P]] =
    validOutputWith(owner, name, required)(ctx => PayloadFactory.auto[C, P](ctx))

  def validInputWith[C, P <: Data](owner: Node[C], name: String, required: Boolean = false)(payload: C => P)(implicit ctx: C): NodeIn[C, ValidIO[P]] =
    new NodeIn(owner, name, NodePortProtocol.Valid, required, namedIO(name)(Flipped(Valid(payload(ctx)))))

  def validOutputWith[C, P <: Data](owner: Node[C], name: String, required: Boolean = false)(payload: C => P)(implicit ctx: C): NodeOut[C, ValidIO[P]] =
    new NodeOut(owner, name, NodePortProtocol.Valid, required, namedIO(name)(Valid(payload(ctx))))

  def decoupledInput[C, P <: Data: ClassTag](owner: Node[C], name: String, required: Boolean = false)(implicit ctx: C): NodeIn[C, DecoupledIO[P]] =
    decoupledInputWith(owner, name, required)(ctx => PayloadFactory.auto[C, P](ctx))

  def decoupledOutput[C, P <: Data: ClassTag](owner: Node[C], name: String, required: Boolean = false)(implicit ctx: C): NodeOut[C, DecoupledIO[P]] =
    decoupledOutputWith(owner, name, required)(ctx => PayloadFactory.auto[C, P](ctx))

  def decoupledInputWith[C, P <: Data](owner: Node[C], name: String, required: Boolean = false)(payload: C => P)(implicit ctx: C): NodeIn[C, DecoupledIO[P]] =
    new NodeIn(owner, name, NodePortProtocol.Decoupled, required, namedIO(name)(Flipped(Decoupled(payload(ctx)))))

  def decoupledOutputWith[C, P <: Data](owner: Node[C], name: String, required: Boolean = false)(payload: C => P)(implicit ctx: C): NodeOut[C, DecoupledIO[P]] =
    new NodeOut(owner, name, NodePortProtocol.Decoupled, required, namedIO(name)(Decoupled(payload(ctx))))

  def vecInput[C, P <: Data: ClassTag](owner: Node[C], name: String, lanes: C => Int, required: Boolean = false)(implicit ctx: C): NodeInVec[C, P] =
    vecInputWith(owner, name, lanes, required)(ctx => PayloadFactory.auto[C, P](ctx))

  def vecOutput[C, P <: Data: ClassTag](owner: Node[C], name: String, lanes: C => Int, required: Boolean = false)(implicit ctx: C): NodeOutVec[C, P] =
    vecOutputWith(owner, name, lanes, required)(ctx => PayloadFactory.auto[C, P](ctx))

  def vecInputWith[C, P <: Data](owner: Node[C], name: String, lanes: C => Int, required: Boolean = false)(payload: C => P)(implicit ctx: C): NodeInVec[C, P] =
    new NodeInVec(owner, name, NodePortProtocol.RawVec, required, namedIO(name)(Input(Vec(lanes(ctx), payload(ctx)))))

  def vecOutputWith[C, P <: Data](owner: Node[C], name: String, lanes: C => Int, required: Boolean = false)(payload: C => P)(implicit ctx: C): NodeOutVec[C, P] =
    new NodeOutVec(owner, name, NodePortProtocol.RawVec, required, namedIO(name)(Output(Vec(lanes(ctx), payload(ctx)))))

  def validVecInput[C, P <: Data: ClassTag](owner: Node[C], name: String, lanes: C => Int, required: Boolean = false)(implicit ctx: C): NodeInVec[C, ValidIO[P]] =
    validVecInputWith(owner, name, lanes, required)(ctx => PayloadFactory.auto[C, P](ctx))

  def validVecOutput[C, P <: Data: ClassTag](owner: Node[C], name: String, lanes: C => Int, required: Boolean = false)(implicit ctx: C): NodeOutVec[C, ValidIO[P]] =
    validVecOutputWith(owner, name, lanes, required)(ctx => PayloadFactory.auto[C, P](ctx))

  def validVecInputWith[C, P <: Data](owner: Node[C], name: String, lanes: C => Int, required: Boolean = false)(payload: C => P)(implicit ctx: C): NodeInVec[C, ValidIO[P]] =
    new NodeInVec(owner, name, NodePortProtocol.ValidVec, required, namedIO(name)(Flipped(Vec(lanes(ctx), Valid(payload(ctx))))))

  def validVecOutputWith[C, P <: Data](owner: Node[C], name: String, lanes: C => Int, required: Boolean = false)(payload: C => P)(implicit ctx: C): NodeOutVec[C, ValidIO[P]] =
    new NodeOutVec(owner, name, NodePortProtocol.ValidVec, required, namedIO(name)(Vec(lanes(ctx), Valid(payload(ctx)))))

  def decoupledVecInput[C, P <: Data: ClassTag](owner: Node[C], name: String, lanes: C => Int, required: Boolean = false)(implicit ctx: C): NodeInVec[C, DecoupledIO[P]] =
    decoupledVecInputWith(owner, name, lanes, required)(ctx => PayloadFactory.auto[C, P](ctx))

  def decoupledVecOutput[C, P <: Data: ClassTag](owner: Node[C], name: String, lanes: C => Int, required: Boolean = false)(implicit ctx: C): NodeOutVec[C, DecoupledIO[P]] =
    decoupledVecOutputWith(owner, name, lanes, required)(ctx => PayloadFactory.auto[C, P](ctx))

  def decoupledVecInputWith[C, P <: Data](owner: Node[C], name: String, lanes: C => Int, required: Boolean = false)(payload: C => P)(implicit ctx: C): NodeInVec[C, DecoupledIO[P]] =
    new NodeInVec(owner, name, NodePortProtocol.DecoupledVec, required, namedIO(name)(Flipped(Vec(lanes(ctx), Decoupled(payload(ctx))))))

  def decoupledVecOutputWith[C, P <: Data](owner: Node[C], name: String, lanes: C => Int, required: Boolean = false)(payload: C => P)(implicit ctx: C): NodeOutVec[C, DecoupledIO[P]] =
    new NodeOutVec(owner, name, NodePortProtocol.DecoupledVec, required, namedIO(name)(Vec(lanes(ctx), Decoupled(payload(ctx)))))
}
