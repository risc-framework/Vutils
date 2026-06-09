package vutils.graph

final case class NodeSelector(values: Vector[(NodeDim, String)] = Vector.empty) {
  def apply(dim: NodeDim): Option[String] =
    values.find(_._1 == dim).map(_._2)

  def getOrElse(dim: NodeDim, default: String): String =
    apply(dim).getOrElse(default)

  def contains(dim: NodeDim): Boolean =
    values.exists(_._1 == dim)

  def canonicalName: String =
    if (values.isEmpty) {
      "default"
    } else {
      values.map { case (dim, value) => s"${dim.name}_${sanitize(value)}" }.mkString("_")
    }

  private def sanitize(value: String): String =
    value.trim
      .replaceAll("[^A-Za-z0-9_]+", "_")
      .replaceAll("_+", "_")
      .stripPrefix("_")
      .stripSuffix("_")
      .toLowerCase
}

object NodeSelector {
  val empty: NodeSelector = new NodeSelector(Vector.empty)

  def apply(values: (NodeDim, String)*): NodeSelector =
    new NodeSelector(values.toVector)
}

final case class NodeConfig(selector: NodeSelector = NodeSelector.empty)

object NodeConfig {
  val empty: NodeConfig = NodeConfig()

  def apply(values: (NodeDim, String)*): NodeConfig =
    NodeConfig(NodeSelector(values: _*))
}
