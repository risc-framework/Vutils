package vutils.buf

import chisel3._
import chisel3.experimental.requireIsChiselType
import chisel3.util.{ Decoupled, PopCount, PriorityEncoder }

class BufQueuedCam[Entry <: Data, Key <: Data](
  entryGen: Entry,
  keyGen: Key,
  depth: Int,
  enqPorts: Int = 1,
  deqPorts: Int = 1,
  lookupPorts: Int = 1,
  backend: BufStorageBackend = BufStorageBackend.Reg,
  priority: BufAgeSelectPolicy = BufAgeSelectPolicy.Youngest
)(matchFn: (Entry, Key) => Bool)
    extends Module {
  require(depth > 0, "BufQueuedCam depth must be positive")
  require(enqPorts >= 0, "BufQueuedCam enqPorts must be non-negative")
  require(deqPorts >= 0, "BufQueuedCam deqPorts must be non-negative")
  require(lookupPorts >= 0, "BufQueuedCam lookupPorts must be non-negative")
  require(enqPorts <= depth, "BufQueuedCam enqPorts must be <= depth")
  require(deqPorts <= depth, "BufQueuedCam deqPorts must be <= depth")
  require(backend == BufStorageBackend.Reg, "BufQueuedCam parallel CAM requires BufStorageBackend.Reg")
  requireIsChiselType(entryGen)
  requireIsChiselType(keyGen)

  private val AddrW = BufUtils.addrWidth(depth)
  private val CntW  = BufUtils.countWidth(depth)

  val io = IO(new Bundle {
    val enq      = Flipped(Vec(enqPorts, Decoupled(entryGen.cloneType)))
    val enqIndex = Output(Vec(enqPorts, UInt(AddrW.W)))

    val deq      = Vec(deqPorts, Decoupled(entryGen.cloneType))
    val deqIndex = Output(Vec(deqPorts, UInt(AddrW.W)))

    val lookup = Vec(lookupPorts, new BufCamLookupPort(entryGen, keyGen, depth))

    val valid = Output(Vec(depth, Bool()))
    val count = Output(UInt(CntW.W))
    val free  = Output(UInt(CntW.W))
    val empty = Output(Bool())
    val full  = Output(Bool())
    val flush = Input(Bool())
  })

  private val storage = Module(new Buf(entryGen, depth, readPorts = deqPorts, writePorts = enqPorts, backend = backend, exposeAll = true))
  private val valid   = RegInit(VecInit(Seq.fill(depth)(false.B)))
  private val head    = RegInit(0.U(AddrW.W))
  private val tail    = RegInit(0.U(AddrW.W))
  private val count   = RegInit(0.U(CntW.W))
  private val zero    = 0.U.asTypeOf(entryGen)

  private def wrapAdd(x: UInt, y: UInt): UInt = {
    val sum = x +& y
    Mux(sum >= depth.U, sum - depth.U, sum)(AddrW - 1, 0)
  }

  private def wrapSub(x: UInt, y: UInt): UInt =
    Mux(x >= y, x - y, x + depth.U - y)(AddrW - 1, 0)

  for (i <- 0 until depth)
    io.valid(i) := valid(i)

  private val deqCanContinue = Wire(Vec(deqPorts + 1, Bool()))
  private val deqFire        = Wire(Vec(deqPorts, Bool()))
  private val deqIdx         = Wire(Vec(deqPorts, UInt(AddrW.W)))

  deqCanContinue(0) := true.B

  for (d <- 0 until deqPorts) {
    deqIdx(d) := wrapAdd(head, d.U)

    storage.io.read(d).en   := count > d.U
    storage.io.read(d).addr := deqIdx(d)

    io.deq(d).valid := count > d.U && deqCanContinue(d)
    io.deq(d).bits  := storage.io.read(d).data
    io.deqIndex(d)  := deqIdx(d)

    deqFire(d)            := io.deq(d).valid && io.deq(d).ready
    deqCanContinue(d + 1) := deqFire(d)
  }

  private val deqCount     = PopCount(deqFire)
  private val freeAfterDeq = depth.U(CntW.W) - count + deqCount

  private val enqUsed = Wire(Vec(enqPorts + 1, UInt(CntW.W)))
  private val enqFire = Wire(Vec(enqPorts, Bool()))
  private val enqIdx  = Wire(Vec(enqPorts, UInt(AddrW.W)))

  enqUsed(0) := 0.U

  for (e <- 0 until enqPorts) {
    io.enq(e).ready := freeAfterDeq > enqUsed(e)

    enqFire(e) := io.enq(e).valid && io.enq(e).ready
    enqIdx(e)  := wrapAdd(tail, enqUsed(e))

    io.enqIndex(e) := enqIdx(e)

    storage.io.write(e).en   := enqFire(e)
    storage.io.write(e).addr := enqIdx(e)
    storage.io.write(e).data := io.enq(e).bits

    enqUsed(e + 1) := enqUsed(e) + enqFire(e).asUInt
  }

  private val enqCount = PopCount(enqFire)

  for (l <- 0 until lookupPorts) {
    val physicalHits = Wire(Vec(depth, Bool()))

    for (i <- 0 until depth) {
      physicalHits(i)      := io.lookup(l).en && valid(i) && matchFn(storage.io.all(i), io.lookup(l).key)
      io.lookup(l).hits(i) := physicalHits(i)
    }

    val orderedHits = Wire(Vec(depth, Bool()))

    for (o <- 0 until depth) {
      val idx = priority match {
        case BufAgeSelectPolicy.Oldest   => wrapAdd(head, o.U)
        case BufAgeSelectPolicy.Youngest => wrapSub(tail, (o + 1).U)
      }

      orderedHits(o) := physicalHits(idx)
    }

    val hit      = orderedHits.asUInt.orR
    val hitOrder = PriorityEncoder(orderedHits)
    val hitIndex = priority match {
      case BufAgeSelectPolicy.Oldest   => wrapAdd(head, hitOrder)
      case BufAgeSelectPolicy.Youngest => wrapSub(tail, hitOrder +& 1.U)
    }

    io.lookup(l).hit   := hit
    io.lookup(l).index := Mux(hit, hitIndex, 0.U)
    io.lookup(l).data  := Mux(hit, storage.io.all(hitIndex), zero)
  }

  for (d <- 0 until deqPorts)
    when(deqFire(d)) {
      valid(deqIdx(d)) := false.B
    }

  for (e <- 0 until enqPorts)
    when(enqFire(e)) {
      valid(enqIdx(e)) := true.B
    }

  head  := wrapAdd(head, deqCount)
  tail  := wrapAdd(tail, enqCount)
  count := count + enqCount - deqCount

  when(io.flush) {
    head  := 0.U
    tail  := 0.U
    count := 0.U

    for (i <- 0 until depth)
      valid(i) := false.B
  }

  io.count := count
  io.free  := depth.U(CntW.W) - count
  io.empty := count === 0.U
  io.full  := count === depth.U
}
