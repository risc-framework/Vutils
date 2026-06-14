package vutils.graph

import chisel3._
import scala.util.Try

sealed trait NodeConnection[C] {
  private[vutils] def ports: Seq[NodePort[C, _ <: Data]]
  private[vutils] def connect(): Unit
}

private object NodeConnectionWarning {
  final private case class Leaf(path: String, data: Data, width: Option[Int]) {
    def shown: String = {
      val p = if (path.isEmpty) "<root>" else path
      width match {
        case Some(w) => s"$p:$w"
        case None    => p
      }
    }
  }

  private def widthOf(data: Data): Option[Int] =
    Try(data.getWidth).toOption

  private def fieldPath(prefix: String, field: String): String =
    if (prefix.isEmpty) field else s"$prefix.$field"

  private def indexPath(prefix: String, index: Int): String =
    if (prefix.isEmpty) s"[$index]" else s"$prefix[$index]"

  private def leavesOf(data: Data, prefix: String = ""): Seq[Leaf] =
    data match {
      case record: Record =>
        record.elements.toSeq.flatMap { case (field, child) =>
          leavesOf(child, fieldPath(prefix, field))
        }

      case vec: Vec[_] =>
        (0 until vec.length).flatMap { i =>
          leavesOf(vec(i).asInstanceOf[Data], indexPath(prefix, i))
        }

      case leaf =>
        Seq(Leaf(prefix, leaf, widthOf(leaf)))
    }

  private def warn(message: String): Unit =
    Console.err.println(s"[vutils.graph][warn] $message")

  private def warnMissing(
    context: String,
    ownerName: String,
    peerName: String,
    ownerOnly: Seq[Leaf]
  ): Unit =
    if (ownerOnly.nonEmpty) {
      warn(
        s"$context: '$ownerName' has ${ownerOnly.length} leaf signal(s) with no peer in '$peerName': " +
          ownerOnly.map(_.shown).mkString(", ")
      )
    }

  private def warnWidthMismatch(
    context: String,
    leftName: String,
    rightName: String,
    mismatches: Seq[(Leaf, Leaf)]
  ): Unit =
    if (mismatches.nonEmpty) {
      warn(
        s"$context: width mismatch between '$leftName' and '$rightName': " +
          mismatches
            .map { case (l, r) =>
              val lw = l.width.map(_.toString).getOrElse("?")
              val rw = r.width.map(_.toString).getOrElse("?")
              val p  = if (l.path.isEmpty) "<root>" else l.path
              s"$p($lw != $rw)"
            }
            .mkString(", ")
      )
    }

  def check(
    context: String,
    leftName: String,
    leftData: Data,
    rightName: String,
    rightData: Data
  ): Unit = {
    val leftLeaves  = leavesOf(leftData)
    val rightLeaves = leavesOf(rightData)
    val leftMap     = leftLeaves.map(leaf => leaf.path -> leaf).toMap
    val rightMap    = rightLeaves.map(leaf => leaf.path -> leaf).toMap
    val leftKeys    = leftMap.keySet
    val rightKeys   = rightMap.keySet
    val leftOnly    = (leftKeys -- rightKeys).toSeq.sorted.flatMap(leftMap.get)
    val rightOnly   = (rightKeys -- leftKeys).toSeq.sorted.flatMap(rightMap.get)
    val common      = (leftKeys intersect rightKeys).toSeq.sorted
    val mismatches  = common.flatMap { path =>
      val l = leftMap(path)
      val r = rightMap(path)
      (l.width, r.width) match {
        case (Some(lw), Some(rw)) if lw != rw => Some(l -> r)
        case _                                => None
      }
    }

    warnMissing(context, leftName, rightName, leftOnly)
    warnMissing(context, rightName, leftName, rightOnly)
    warnWidthMismatch(context, leftName, rightName, mismatches)
  }
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

  override private[vutils] def connect(): Unit = {
    NodeConnectionWarning.check(
      context = "NodeEdge",
      leftName = source.fullName,
      leftData = source.data,
      rightName = sink.fullName,
      rightData = sink.data
    )

    sink.data <> source.data
  }
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

  override private[vutils] def connect(): Unit = {
    NodeConnectionWarning.check(
      context = "NodeExpose",
      leftName = left.fullName,
      leftData = left.data,
      rightName = right.fullName,
      rightData = right.data
    )

    left.data <> right.data
  }
}
