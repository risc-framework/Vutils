package vutils.fsm

import chisel3._
import chisel3.util.DecoupledIO

final class ElasticRouteBuilder[T <: Data, N <: Data] private[fsm] (
  private val graph: ElasticGraphBuilder[T, N],
  private val from: ElasticStage[T, N]
) {
  def to(stage: ElasticStage[T, N], when: Bool = true.B): ElasticMove =
    graph.connect(from, stage, trigger = when)

  def toMap(stage: ElasticStage[T, N], when: Bool = true.B)(body: T => Unit): ElasticMove =
    graph.connectMap(from, stage, trigger = when)(body)

  def request[R <: Data](
    req: DecoupledIO[R],
    wait: ElasticStage[T, N],
    when: Bool = true.B
  )(
    body: R => Unit
  ): ElasticMove =
    graph.request(from, req, wait, trigger = when)(body)
}
