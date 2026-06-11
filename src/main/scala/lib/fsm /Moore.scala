package vutils.fsm

import chisel3._
import chisel3.util.MuxCase

final case class MooreState[S <: Data](
  state: S,
  nextState: S
)

trait Moore { this: Module =>
  val sType: ChiselEnum

  type State = sType.Type

  final protected class Event private[Moore] (
    val from: State,
    val to: State,
    val body: () => Unit
  )

  type Trigger = Bool
  type Rule    = (Trigger, Event)

  final protected def event(from: State, to: State)(body: => Unit): Event =
    new Event(from, to, () => body)

  final protected def event(from: State, to: State): Event =
    new Event(from, to, () => ())

  final protected def trans(
    start: State,
    clear: Bool = false.B,
    enable: Bool = true.B
  )(
    rules: Rule*
  ): MooreState[State] = {
    val stateReg = RegInit(start)

    val hits = rules.map { case (trigger, event) =>
      enable && trigger && stateReg === event.from
    }

    val priorHits = hits.scanLeft(false.B)(_ || _).dropRight(1)

    val fires = hits.zip(priorHits).map { case (hit, prior) =>
      hit && !prior
    }

    val next = MuxCase(
      stateReg,
      rules.zip(fires).map { case ((_, event), fire) =>
        fire -> event.to
      }
    )

    when(clear) {
      stateReg := start
    }.elsewhen(enable) {
      stateReg := next
    }

    rules.zip(fires).foreach { case ((_, event), fire) =>
      when(!clear && fire) {
        event.body()
      }
    }

    MooreState(
      state = stateReg,
      nextState = next
    )
  }
}
