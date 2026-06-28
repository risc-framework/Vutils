package examples

import vutils._
import vutils.buf._
import chisel3._

class QueueCamEntry extends Bundle {
  val addr = UInt(32.W)
  val data = UInt(32.W)
  val mask = UInt(4.W)
}

class QueueCamLookupKey extends Bundle {
  val addr = UInt(32.W)
  val mask = UInt(4.W)
}

class QueueCamExample extends Module {
  val io = IO(new Bundle {
    val flush = Input(Bool())

    val enqValid = Input(Bool())
    val enqReady = Output(Bool())
    val enqAddr  = Input(UInt(32.W))
    val enqData  = Input(UInt(32.W))
    val enqMask  = Input(UInt(4.W))
    val enqIndex = Output(UInt(BufUtils.addrWidth(8).W))

    val deqReady = Input(Bool())
    val deqValid = Output(Bool())
    val deqAddr  = Output(UInt(32.W))
    val deqData  = Output(UInt(32.W))
    val deqMask  = Output(UInt(4.W))
    val deqIndex = Output(UInt(BufUtils.addrWidth(8).W))

    val lookupEn      = Input(Bool())
    val lookupAddr    = Input(UInt(32.W))
    val lookupMask    = Input(UInt(4.W))
    val lookupHit     = Output(Bool())
    val lookupIndex   = Output(UInt(BufUtils.addrWidth(8).W))
    val lookupData    = Output(UInt(32.W))
    val lookupMaskOut = Output(UInt(4.W))
  })

  private val qcam = Module(
    new BufQueuedCam(
      entryGen = new QueueCamEntry,
      keyGen = new QueueCamLookupKey,
      depth = 8,
      enqPorts = 1,
      deqPorts = 1,
      lookupPorts = 1,
      backend = BufStorageBackend.Reg,
      priority = BufAgeSelectPolicy.Youngest
    )((entry: QueueCamEntry, key: QueueCamLookupKey) => entry.addr === key.addr && (entry.mask & key.mask).orR)
  )

  val enqEntry = Wire(new QueueCamEntry)
  enqEntry.addr := io.enqAddr
  enqEntry.data := io.enqData
  enqEntry.mask := io.enqMask

  val lookupKey = Wire(new QueueCamEntry)
  lookupKey.addr := io.lookupAddr
  lookupKey.data := 0.U
  lookupKey.mask := io.lookupMask

  qcam.io.flush := io.flush

  qcam.io.enq(0).valid := io.enqValid
  qcam.io.enq(0).bits  := enqEntry
  io.enqReady          := qcam.io.enq(0).ready
  io.enqIndex          := qcam.io.enqIndex(0)

  qcam.io.deq(0).ready := io.deqReady
  io.deqValid          := qcam.io.deq(0).valid
  io.deqAddr           := qcam.io.deq(0).bits.addr
  io.deqData           := qcam.io.deq(0).bits.data
  io.deqMask           := qcam.io.deq(0).bits.mask
  io.deqIndex          := qcam.io.deqIndex(0)

  qcam.io.lookup(0).en  := io.lookupEn
  qcam.io.lookup(0).key := lookupKey

  io.lookupHit     := qcam.io.lookup(0).hit
  io.lookupIndex   := qcam.io.lookup(0).index
  io.lookupData    := qcam.io.lookup(0).data.data
  io.lookupMaskOut := qcam.io.lookup(0).data.mask
}

object QueueCamExample extends App {
  DesignEmitter.emit(
    gen = new QueueCamExample,
    filename = "queue_cam",
    target = SystemVerilog,
    info = true,
    lowering = true,
  )
}
