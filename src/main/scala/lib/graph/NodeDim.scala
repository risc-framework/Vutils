package vutils.graph

import scala.collection.mutable

final class NodeDim private[vutils] (
  val nodeType: NodeType,
  val name: String
) {
  self =>

  trait Impl extends NodeDimensionImpl {
    final override def dim: NodeDim = self
  }

  class Registry[I <: Impl](
    default: Option[String] = None
  ) extends NodeDimensionRegistry[I](self, default)

  override def toString: String =
    s"${nodeType.name}.$name"

  override def equals(that: Any): Boolean =
    that match {
      case other: NodeDim => nodeType == other.nodeType && name == other.name
      case _              => false
    }

  override def hashCode(): Int =
    31 * nodeType.hashCode() + name.hashCode()
}

abstract class NodeDims(nodeName: String) {
  final val Type: NodeType = NodeType(nodeName)

  final protected def dim(name: String): NodeDim =
    new NodeDim(Type, name)
}

trait NodeDimensionImpl {
  def value: String
  def dim: NodeDim

  final def nodeType: NodeType =
    dim.nodeType

  def name: String =
    value
}

class NodeDimensionRegistry[I <: NodeDimensionImpl](
  val dim: NodeDim,
  val default: Option[String] = None
) {
  private val impls = mutable.LinkedHashMap.empty[String, I]

  final def register(impl: I): I = {
    require(
      impl.dim == dim,
      s"NodeDimensionRegistry '$dim' cannot register impl '${impl.name}' for dim '${impl.dim}'"
    )

    require(
      !impls.contains(impl.name),
      s"NodeDimensionRegistry '$dim' already has impl '${impl.name}'"
    )

    impls += impl.name -> impl
    impl
  }

  final def get(name: String): Option[I] =
    impls.get(name)

  final def getOrThrow(name: String): I =
    impls.getOrElse(
      name,
      throw new NoSuchElementException(
        s"NodeDimensionRegistry '$dim' has no impl '$name'. Available: ${impls.keys.mkString(", ")}"
      )
    )

  final def select(cfg: NodeConfig): I = {
    val selected = cfg.selector(dim).orElse(default).getOrElse {
      throw new NoSuchElementException(
        s"NodeDimensionRegistry '$dim' cannot select implementation because cfg has no selector for '${dim.name}' and no default was provided"
      )
    }

    getOrThrow(selected)
  }

  final def values: Seq[I] =
    impls.values.toSeq
}

trait RegisteredNodeUtils[I <: NodeDimensionImpl] {
  def utils: I
  def registry: NodeDimensionRegistry[I]

  final lazy val registered: I =
    registry.register(utils)
}
