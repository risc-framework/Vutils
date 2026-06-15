package vutils.fsm

import chisel3._
import chisel3.util.{ Decoupled, Queue, log2Ceil }

private[fsm] class ElasticQueue[T <: Data](
  gen: T,
  entries: Int,
  pipe: Boolean = false,
  flow: Boolean = false,
  useSyncReadMem: Boolean = false
) extends Module {
  require(entries > 0, "ElasticQueue entries must be > 0")

  val io = IO(new Bundle {
    val clear = Input(Bool())
    val enq   = Flipped(Decoupled(gen))
    val deq   = Decoupled(gen)
    val count = Output(UInt(log2Ceil(entries + 1).W))
  })

  private val q = withReset(reset.asBool || io.clear) {
    Module(new Queue(gen, entries, pipe = pipe, flow = flow, useSyncReadMem = useSyncReadMem))
  }

  q.io.enq.valid := io.enq.valid && !io.clear
  q.io.enq.bits  := io.enq.bits
  q.io.deq.ready := io.deq.ready && !io.clear

  io.enq.ready := q.io.enq.ready && !io.clear
  io.deq.valid := q.io.deq.valid && !io.clear
  io.deq.bits  := q.io.deq.bits
  io.count     := Mux(io.clear, 0.U(log2Ceil(entries + 1).W), q.io.count)
}
