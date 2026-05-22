package examples

import vutils._
import vutils.datatype._
import chisel3._

class FixedPoint32MAC extends Module {
  val io = IO(new Bundle {
    val in0 = Input(FixedPoint(32.W, 16.BP))
    val in1 = Input(FixedPoint(32.W, 16.BP))
    val in2 = Input(FixedPoint(32.W, 16.BP))
    val out = Output(FixedPoint(32.W, 16.BP))
  })

  io.out := (io.in0 + io.in1) * io.in2
}

object FixedPoint32MACExample extends App {
  DesignEmitter.emit(
    gen = new FixedPoint32MAC,
    filename = "fixedpoint32_mac",
    target = SystemVerilog,
    info = true,
    lowering = true,
  )
}
