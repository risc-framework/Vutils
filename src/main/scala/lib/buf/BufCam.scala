package vutils.buf

import chisel3._
import chisel3.experimental.requireIsChiselType
import chisel3.util.Mux1H

class BufCam[Entry <: Data, Key <: Data](
  entryGen: Entry,
  keyGen: Key,
  depth: Int,
  lookupPorts: Int = 1,
  writePorts: Int = 1,
  removePorts: Int = 1,
  backend: BufStorageBackend = BufStorageBackend.Reg,
  select: BufIndexSelectPolicy = BufIndexSelectPolicy.LowIndex
)(matchFn: (Entry, Key) => Bool)
    extends Module {
  require(depth > 0, "BufCam depth must be positive")
  require(lookupPorts >= 0, "BufCam lookupPorts must be non-negative")
  require(writePorts >= 0, "BufCam writePorts must be non-negative")
  require(removePorts >= 0, "BufCam removePorts must be non-negative")
  require(backend == BufStorageBackend.Reg, "BufCam parallel lookup requires BufStorageBackend.Reg")
  requireIsChiselType(entryGen)
  requireIsChiselType(keyGen)

  private val AddrW = BufUtils.addrWidth(depth)

  val io = IO(new Bundle {
    val lookup = Vec(lookupPorts, new BufCamLookupPort(entryGen, keyGen, depth))
    val write  = Vec(writePorts, new BufCamWritePort(entryGen, depth))
    val remove = Vec(removePorts, new BufCamRemovePort(depth))
    val valid  = Output(Vec(depth, Bool()))
    val flush  = Input(Bool())
  })

  private val storage = Module(new Buf(entryGen, depth, readPorts = 0, writePorts = writePorts, backend = backend, exposeAll = true))
  private val valid   = RegInit(VecInit(Seq.fill(depth)(false.B)))
  private val zero    = 0.U.asTypeOf(entryGen)

  for (i <- 0 until depth)
    io.valid(i) := valid(i)

  for (w <- 0 until writePorts) {
    storage.io.write(w).en   := io.write(w).en && io.write(w).valid
    storage.io.write(w).addr := io.write(w).index
    storage.io.write(w).data := io.write(w).data

    when(io.write(w).en) {
      valid(io.write(w).index) := io.write(w).valid
    }
  }

  for (r <- 0 until removePorts)
    when(io.remove(r).en) {
      valid(io.remove(r).index) := false.B
    }

  when(io.flush) {
    for (i <- 0 until depth)
      valid(i) := false.B
  }

  for (l <- 0 until lookupPorts) {
    val physicalHits = Wire(Vec(depth, Bool()))

    for (i <- 0 until depth) {
      physicalHits(i)      := io.lookup(l).en && valid(i) && matchFn(storage.io.all(i), io.lookup(l).key)
      io.lookup(l).hits(i) := physicalHits(i)
    }

    val orderedHits = Wire(Vec(depth, Bool()))
    val orderedData = Wire(Vec(depth, entryGen.cloneType))
    val orderedIdx  = Wire(Vec(depth, UInt(AddrW.W)))

    for (o <- 0 until depth) {
      val idx = select match {
        case BufIndexSelectPolicy.LowIndex  => o
        case BufIndexSelectPolicy.HighIndex => depth - 1 - o
      }

      orderedHits(o) := physicalHits(idx)
      orderedData(o) := storage.io.all(idx)
      orderedIdx(o)  := idx.U(AddrW.W)
    }

    val hit = orderedHits.asUInt.orR

    io.lookup(l).hit   := hit
    io.lookup(l).index := Mux(hit, Mux1H(orderedHits, orderedIdx), 0.U)
    io.lookup(l).data  := Mux(hit, Mux1H(orderedHits, orderedData), zero)
  }
}
