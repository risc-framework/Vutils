package examples

import vutils._
import vutils.fsm.Moore
import chisel3._

object TraficLightColor extends ChiselEnum {
  val Red, Green, Yellow = Value
}

class TrafficLight extends Module with Moore {
  val io = IO(new Bundle {
    val clear     = Input(Bool())
    val enable    = Input(Bool())
    val step      = Input(Bool())
    val red       = Output(Bool())
    val green     = Output(Bool())
    val yellow    = Output(Bool())
    val state     = Output(TraficLightColor())
    val nextState = Output(TraficLightColor())
  })

  private val fsm = moore(TraficLightColor.Red, clear = io.clear, enable = io.enable) { g =>
    import g._

    val Red    = state(TraficLightColor.Red)
    val Green  = state(TraficLightColor.Green)
    val Yellow = state(TraficLightColor.Yellow)

    trans(Red, Green, io.step)
    trans(Green, Yellow, io.step)
    trans(Yellow, Red, io.step)
  }

  io.red       := fsm(TraficLightColor.Red).active
  io.green     := fsm(TraficLightColor.Green).active
  io.yellow    := fsm(TraficLightColor.Yellow).active
  io.state     := fsm.state
  io.nextState := fsm.nextState
}

object TrafficLightExample extends App {
  DesignEmitter.emit(
    gen = new TrafficLight,
    filename = "traffic_light",
    target = SystemVerilog,
    info = true,
    lowering = true,
  )
}
