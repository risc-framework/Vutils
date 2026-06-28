package vutils.buf

import chisel3._
import chisel3.experimental.requireIsChiselType
import chisel3.util.{ Decoupled, PopCount }

class BufCircularBuffer[T <: Data](
  gen: T,
  depth: Int,
  allocPorts: Int = 1,
  deqPorts: Int = 1,
  readPorts: Int = 0,
  writePorts: Int = 0,
  backend: BufStorageBackend = BufStorageBackend.Reg
) extends Module {
  require(depth > 0, "BufCircularBuffer depth must be positive")
  require(allocPorts >= 0, "allocPorts must be non-negative")
  require(deqPorts >= 0, "deqPorts must be non-negative")
  require(readPorts >= 0, "readPorts must be non-negative")
  require(writePorts >= 0, "writePorts must be non-negative")
  require(backend.readLatency == 0, "BufCircularBuffer requires zero-latency read backend")
  requireIsChiselType(gen)

  private val AddrW = BufUtils.addrWidth(depth)
  private val CntW  = BufUtils.countWidth(depth)

  val io = IO(new Bundle {
    val alloc      = Flipped(Vec(allocPorts, Decoupled(gen.cloneType)))
    val allocIndex = Output(Vec(allocPorts, UInt(AddrW.W)))

    val deq      = Vec(deqPorts, Decoupled(gen.cloneType))
    val deqIndex = Output(Vec(deqPorts, UInt(AddrW.W)))

    val read  = Vec(readPorts, new BufReadPort(gen, depth))
    val write = Vec(writePorts, new BufWritePort(gen, depth))

    val count = Output(UInt(CntW.W))
    val free  = Output(UInt(CntW.W))
    val empty = Output(Bool())
    val full  = Output(Bool())
    val flush = Input(Bool())
  })

  private val storage = Module(new Buf(gen, depth, readPorts = deqPorts + readPorts, writePorts = allocPorts + writePorts, backend = backend, exposeAll = false))
  private val head    = RegInit(0.U(AddrW.W))
  private val tail    = RegInit(0.U(AddrW.W))
  private val count   = RegInit(0.U(CntW.W))

  private def wrapAdd(x: UInt, y: UInt): UInt = {
    val sum = x +& y
    Mux(sum >= depth.U, sum - depth.U, sum)(AddrW - 1, 0)
  }

  private val deqCanContinue = Wire(Vec(deqPorts + 1, Bool()))
  private val deqFire        = Wire(Vec(deqPorts, Bool()))
  private val allocFire      = Wire(Vec(allocPorts, Bool()))

  deqCanContinue(0) := true.B

  for (d <- 0 until deqPorts) {
    val idx = wrapAdd(head, d.U)

    storage.io.read(d).en   := count > d.U
    storage.io.read(d).addr := idx

    io.deq(d).valid := count > d.U && deqCanContinue(d)
    io.deq(d).bits  := storage.io.read(d).data
    io.deqIndex(d)  := idx

    deqFire(d)            := io.deq(d).valid && io.deq(d).ready
    deqCanContinue(d + 1) := deqFire(d)
  }

  for (r <- 0 until readPorts) {
    val storageReadIdx = deqPorts + r

    storage.io.read(storageReadIdx).en   := io.read(r).en
    storage.io.read(storageReadIdx).addr := io.read(r).addr
    io.read(r).data                      := storage.io.read(storageReadIdx).data
  }

  private val deqCount     = PopCount(deqFire)
  private val freeAfterDeq = depth.U(CntW.W) - count + deqCount
  private val allocUsed    = Wire(Vec(allocPorts + 1, UInt(CntW.W)))

  allocUsed(0) := 0.U

  for (a <- 0 until allocPorts) {
    io.alloc(a).ready := freeAfterDeq > allocUsed(a)

    allocFire(a)     := io.alloc(a).valid && io.alloc(a).ready
    io.allocIndex(a) := wrapAdd(tail, allocUsed(a))

    allocUsed(a + 1) := allocUsed(a) + allocFire(a).asUInt

    storage.io.write(a).en   := allocFire(a)
    storage.io.write(a).addr := io.allocIndex(a)
    storage.io.write(a).data := io.alloc(a).bits
  }

  for (w <- 0 until writePorts) {
    val storageWriteIdx = allocPorts + w

    storage.io.write(storageWriteIdx).en   := io.write(w).en
    storage.io.write(storageWriteIdx).addr := io.write(w).addr
    storage.io.write(storageWriteIdx).data := io.write(w).data
  }

  private val allocCount = PopCount(allocFire)

  head  := wrapAdd(head, deqCount)
  tail  := wrapAdd(tail, allocCount)
  count := count + allocCount - deqCount

  when(io.flush) {
    head  := 0.U
    tail  := 0.U
    count := 0.U
  }

  io.count := count
  io.free  := depth.U(CntW.W) - count
  io.empty := count === 0.U
  io.full  := count === depth.U
}
