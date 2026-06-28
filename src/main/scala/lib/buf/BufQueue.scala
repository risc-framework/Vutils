package vutils.buf

import chisel3._
import chisel3.experimental.requireIsChiselType
import chisel3.util.{ DecoupledIO, QueueIO }

class BufQueue[T <: Data](
  gen: T,
  entries: Int,
  backend: BufStorageBackend = BufStorageBackend.Reg,
  pipe: Boolean = false,
  flow: Boolean = false,
  hasFlush: Boolean = true
) extends Module {
  require(entries > 0, "BufQueue entries must be positive")
  require(backend.readLatency == 0, "BufQueue requires zero-latency read backend")
  requireIsChiselType(gen)

  val io = IO(new QueueIO(gen, entries, hasFlush))

  private val AddrW = BufUtils.addrWidth(entries)
  private val CntW  = BufUtils.countWidth(entries)

  private val storage = Module(new Buf(gen, entries, readPorts = 1, writePorts = 1, backend = backend, exposeAll = false))
  private val enqPtr  = RegInit(0.U(AddrW.W))
  private val deqPtr  = RegInit(0.U(AddrW.W))
  private val count   = RegInit(0.U(CntW.W))

  private def wrapInc(x: UInt): UInt =
    Mux(x === (entries - 1).U, 0.U, x + 1.U)(AddrW - 1, 0)

  private val empty = count === 0.U
  private val full  = count === entries.U

  io.enq.ready := !full
  io.deq.valid := !empty

  storage.io.read(0).en   := !empty
  storage.io.read(0).addr := deqPtr
  io.deq.bits             := storage.io.read(0).data

  private val doEnq = WireDefault(io.enq.valid && io.enq.ready)
  private val doDeq = WireDefault(io.deq.valid && io.deq.ready)

  if (flow) {
    when(io.enq.valid) {
      io.deq.valid := true.B
    }

    when(empty) {
      io.deq.bits := io.enq.bits
      doDeq       := false.B

      when(io.deq.ready) {
        doEnq := false.B
      }
    }
  }

  if (pipe) {
    when(io.deq.ready) {
      io.enq.ready := true.B
    }
  }

  storage.io.write(0).en   := doEnq
  storage.io.write(0).addr := enqPtr
  storage.io.write(0).data := io.enq.bits

  when(doEnq) {
    enqPtr := wrapInc(enqPtr)
  }

  when(doDeq) {
    deqPtr := wrapInc(deqPtr)
  }

  when(doEnq =/= doDeq) {
    count := Mux(doEnq, count + 1.U, count - 1.U)
  }

  if (hasFlush) {
    when(io.flush.get) {
      enqPtr := 0.U
      deqPtr := 0.U
      count  := 0.U
    }
  }

  io.count := count
}

object BufQueue {
  def apply[T <: Data](
    enq: DecoupledIO[T],
    entries: Int,
    backend: BufStorageBackend = BufStorageBackend.Reg,
    pipe: Boolean = false,
    flow: Boolean = false,
    hasFlush: Boolean = true
  ): DecoupledIO[T] = {
    val q = Module(new BufQueue(chiselTypeOf(enq.bits), entries, backend, pipe, flow, hasFlush))
    q.io.enq <> enq

    if (hasFlush) {
      q.io.flush.get := false.B
    }

    q.io.deq
  }
}
