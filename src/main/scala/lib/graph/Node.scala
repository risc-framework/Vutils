package vutils.graph

import chisel3._
import chisel3.util.{ DecoupledIO, ValidIO }
import scala.collection.mutable
import scala.reflect.ClassTag

abstract class Node[C](
  val nodeName: String
)(implicit val nodeContext: C)
    extends Module {
  protected def cfg: NodeConfig = NodeConfig.empty

  final def nodeType: NodeType = NodeType(nodeName)

  private val registeredPorts       = mutable.ArrayBuffer.empty[NodePort[C, _ <: Data]]
  private val registeredSubnodes    = mutable.ArrayBuffer.empty[Node[C]]
  private val registeredConnections = mutable.ArrayBuffer.empty[NodeConnection[C]]

  override def desiredName: String =
    s"${nodeName}_${cfg.selector.canonicalName}"

  private def normalizeName(name: String): String =
    name.trim

  private def register[Port <: NodePort[C, _ <: Data]](port: Port): Port = {
    require(
      !registeredPorts.exists(_.name == port.name),
      s"Node '$nodeName' already has port '${port.name}'"
    )

    registeredPorts += port
    port
  }

  final protected def in[P <: Data: ClassTag](implicit name: sourcecode.Name): NodeIn[C, P] =
    register(NodePort.input[C, P](this, normalizeName(name.value)))

  final protected def out[P <: Data: ClassTag](implicit name: sourcecode.Name): NodeOut[C, P] =
    register(NodePort.output[C, P](this, normalizeName(name.value)))

  final protected def inWith[P <: Data](payload: C => P)(implicit name: sourcecode.Name): NodeIn[C, P] =
    register(NodePort.inputWith[C, P](this, normalizeName(name.value))(payload))

  final protected def outWith[P <: Data](payload: C => P)(implicit name: sourcecode.Name): NodeOut[C, P] =
    register(NodePort.outputWith[C, P](this, normalizeName(name.value))(payload))

  final protected def inV[P <: Data: ClassTag](implicit name: sourcecode.Name): NodeIn[C, ValidIO[P]] =
    register(NodePort.validInput[C, P](this, normalizeName(name.value)))

  final protected def outV[P <: Data: ClassTag](implicit name: sourcecode.Name): NodeOut[C, ValidIO[P]] =
    register(NodePort.validOutput[C, P](this, normalizeName(name.value)))

  final protected def inVWith[P <: Data](payload: C => P)(implicit name: sourcecode.Name): NodeIn[C, ValidIO[P]] =
    register(NodePort.validInputWith[C, P](this, normalizeName(name.value))(payload))

  final protected def outVWith[P <: Data](payload: C => P)(implicit name: sourcecode.Name): NodeOut[C, ValidIO[P]] =
    register(NodePort.validOutputWith[C, P](this, normalizeName(name.value))(payload))

  final protected def inD[P <: Data: ClassTag](implicit name: sourcecode.Name): NodeIn[C, DecoupledIO[P]] =
    register(NodePort.decoupledInput[C, P](this, normalizeName(name.value)))

  final protected def outD[P <: Data: ClassTag](implicit name: sourcecode.Name): NodeOut[C, DecoupledIO[P]] =
    register(NodePort.decoupledOutput[C, P](this, normalizeName(name.value)))

  final protected def inDWith[P <: Data](payload: C => P)(implicit name: sourcecode.Name): NodeIn[C, DecoupledIO[P]] =
    register(NodePort.decoupledInputWith[C, P](this, normalizeName(name.value))(payload))

  final protected def outDWith[P <: Data](payload: C => P)(implicit name: sourcecode.Name): NodeOut[C, DecoupledIO[P]] =
    register(NodePort.decoupledOutputWith[C, P](this, normalizeName(name.value))(payload))

  final protected def inVec[P <: Data: ClassTag](lanes: C => Int)(implicit name: sourcecode.Name): NodeInVec[C, P] =
    register(NodePort.vecInput[C, P](this, normalizeName(name.value), lanes))

  final protected def outVec[P <: Data: ClassTag](lanes: C => Int)(implicit name: sourcecode.Name): NodeOutVec[C, P] =
    register(NodePort.vecOutput[C, P](this, normalizeName(name.value), lanes))

  final protected def inVecWith[P <: Data](lanes: C => Int)(payload: C => P)(implicit name: sourcecode.Name): NodeInVec[C, P] =
    register(NodePort.vecInputWith[C, P](this, normalizeName(name.value), lanes)(payload))

  final protected def outVecWith[P <: Data](lanes: C => Int)(payload: C => P)(implicit name: sourcecode.Name): NodeOutVec[C, P] =
    register(NodePort.vecOutputWith[C, P](this, normalizeName(name.value), lanes)(payload))

  final protected def inVVec[P <: Data: ClassTag](lanes: C => Int)(implicit name: sourcecode.Name): NodeInVec[C, ValidIO[P]] =
    register(NodePort.validVecInput[C, P](this, normalizeName(name.value), lanes))

  final protected def outVVec[P <: Data: ClassTag](lanes: C => Int)(implicit name: sourcecode.Name): NodeOutVec[C, ValidIO[P]] =
    register(NodePort.validVecOutput[C, P](this, normalizeName(name.value), lanes))

  final protected def inVVecWith[P <: Data](lanes: C => Int)(payload: C => P)(implicit name: sourcecode.Name): NodeInVec[C, ValidIO[P]] =
    register(NodePort.validVecInputWith[C, P](this, normalizeName(name.value), lanes)(payload))

  final protected def outVVecWith[P <: Data](lanes: C => Int)(payload: C => P)(implicit name: sourcecode.Name): NodeOutVec[C, ValidIO[P]] =
    register(NodePort.validVecOutputWith[C, P](this, normalizeName(name.value), lanes)(payload))

  final protected def inDVec[P <: Data: ClassTag](lanes: C => Int)(implicit name: sourcecode.Name): NodeInVec[C, DecoupledIO[P]] =
    register(NodePort.decoupledVecInput[C, P](this, normalizeName(name.value), lanes))

  final protected def outDVec[P <: Data: ClassTag](lanes: C => Int)(implicit name: sourcecode.Name): NodeOutVec[C, DecoupledIO[P]] =
    register(NodePort.decoupledVecOutput[C, P](this, normalizeName(name.value), lanes))

  final protected def inDVecWith[P <: Data](lanes: C => Int)(payload: C => P)(implicit name: sourcecode.Name): NodeInVec[C, DecoupledIO[P]] =
    register(NodePort.decoupledVecInputWith[C, P](this, normalizeName(name.value), lanes)(payload))

  final protected def outDVecWith[P <: Data](lanes: C => Int)(payload: C => P)(implicit name: sourcecode.Name): NodeOutVec[C, DecoupledIO[P]] =
    register(NodePort.decoupledVecOutputWith[C, P](this, normalizeName(name.value), lanes)(payload))

  final protected def subnode[N <: Node[C]](gen: => N): N = {
    val node = Module(gen)
    registeredSubnodes += node
    node
  }

  final protected def subnodes: Seq[Node[C]] =
    registeredSubnodes.toSeq

  private def visibleOwner(owner: Node[C]): Boolean =
    owner == this || registeredSubnodes.contains(owner)

  private def validateConnection(connection: NodeConnection[C]): Unit = {
    connection.ports.foreach { port =>
      require(
        visibleOwner(port.owner),
        s"Node '$nodeName' cannot connect '${port.fullName}' because '${port.owner.nodeName}' is neither this node nor a registered subnode"
      )
    }

    connection match {
      case expose: NodeExpose[C, _] =>
        val hasParentEndpoint = expose.ports.exists(_.owner == this)

        require(
          hasParentEndpoint,
          s"Node '$nodeName' cannot expose '${expose.ports.map(_.fullName).mkString(" <-> ")}' because neither endpoint is owned by this node"
        )

      case _ =>
    }
  }

  final protected def link(connections: NodeConnection[C]*): Unit =
    connections.foreach { connection =>
      validateConnection(connection)
      registeredConnections += connection
      connection.connect()
    }

  final protected def expose[PortData <: Data](
    parentPort: NodePort[C, PortData],
    childPort: NodePort[C, PortData]
  ): Unit = {
    require(
      parentPort.owner == this,
      s"Node '$nodeName' cannot expose through '${parentPort.fullName}' because it is not owned by this node"
    )

    require(
      registeredSubnodes.contains(childPort.owner),
      s"Node '$nodeName' cannot expose '${childPort.fullName}' because '${childPort.owner.nodeName}' is not a registered subnode"
    )

    require(
      parentPort.role == childPort.role,
      s"Node '$nodeName' cannot expose '${childPort.fullName}' as '${parentPort.fullName}': role mismatch ${childPort.role.name} != ${parentPort.role.name}"
    )

    require(
      parentPort.protocol == childPort.protocol,
      s"Node '$nodeName' cannot expose '${childPort.fullName}' as '${parentPort.fullName}': protocol mismatch ${childPort.protocol.name} != ${parentPort.protocol.name}"
    )

    parentPort.data <> childPort.data
  }

  final private[vutils] def graphPorts: Seq[NodePort[C, _ <: Data]] =
    registeredPorts.toSeq

  final private[vutils] def graphSubnodes: Seq[Node[C]] =
    registeredSubnodes.toSeq

  final private[vutils] def graphConnections: Seq[NodeConnection[C]] =
    registeredConnections.toSeq

  final private[vutils] def graphEdges: Seq[NodeEdge[C, _ <: Data]] =
    registeredConnections.collect { case edge: NodeEdge[C, _] => edge }.toSeq
}
