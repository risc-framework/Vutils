package vutils.fsm

import chisel3._
import chisel3.util.DecoupledIO

final class ElasticRouteBuilder[T <: Data, N <: Data] private[fsm] (
  private val graph: ElasticGraphBuilder[T, N],
  private val from: ElasticStage[T, N]
) {
  private def checkStageEdge(edge: (ElasticStage[T, N], ElasticStage[T, N])): Unit =
    require(edge._1.id == from.id, s"route source mismatch: route starts at ${from.node}, but edge starts at ${edge._1.node}")

  private def checkRequestEdge[R <: Data](edge: ((ElasticStage[T, N], DecoupledIO[R]), ElasticStage[T, N])): Unit =
    require(edge._1._1.id == from.id, s"route source mismatch: route starts at ${from.node}, but edge starts at ${edge._1._1.node}")

  def to(edge: (ElasticStage[T, N], ElasticStage[T, N]), when: Bool = true.B): ElasticMove = {
    checkStageEdge(edge)
    graph.connect(edge, trigger = when)
  }

  def toMap(edge: (ElasticStage[T, N], ElasticStage[T, N]), when: Bool = true.B)(body: T => Unit): ElasticMove = {
    checkStageEdge(edge)
    graph.connectMap(edge, trigger = when)(body)
  }

  def request[R <: Data](
    edge: ((ElasticStage[T, N], DecoupledIO[R]), ElasticStage[T, N]),
    when: Bool = true.B
  )(
    body: R => Unit
  ): ElasticMove = {
    checkRequestEdge(edge)
    graph.request(edge, trigger = when)(body)
  }

  def requestMap[R <: Data](
    edge: ((ElasticStage[T, N], DecoupledIO[R]), ElasticStage[T, N]),
    when: Bool = true.B
  )(
    reqBody: R => Unit
  )(
    waitBody: T => Unit
  ): ElasticMove = {
    checkRequestEdge(edge)
    graph.requestMap(edge, trigger = when)(reqBody)(waitBody)
  }
}
