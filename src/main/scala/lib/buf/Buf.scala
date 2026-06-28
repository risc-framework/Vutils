package vutils.buf

import chisel3._
import chisel3.experimental.requireIsChiselType

class Buf[T <: Data](
  gen: T,
  depth: Int,
  readPorts: Int = 0,
  writePorts: Int = 0,
  backend: BufStorageBackend = BufStorageBackend.Reg,
  exposeAll: Boolean = false
) extends Module {
  require(depth > 0, "Buf depth must be positive")
  require(readPorts >= 0, "Buf readPorts must be non-negative")
  require(writePorts >= 0, "Buf writePorts must be non-negative")
  require(!exposeAll || backend.supportsAllView, "Buf exposeAll requires a backend with all-entry view support")
  requireIsChiselType(gen)

  val io = IO(new Bundle {
    val read     = Vec(readPorts, new BufReadPort(gen, depth))
    val write    = Vec(writePorts, new BufWritePort(gen, depth))
    val all      = Output(Vec(depth, gen.cloneType))
    val allValid = Output(Bool())
  })

  private val zero = 0.U.asTypeOf(gen)

  backend match {
    case BufStorageBackend.Reg =>
      val storage = Reg(Vec(depth, gen.cloneType))

      for (w <- 0 until writePorts)
        when(io.write(w).en) {
          storage(io.write(w).addr) := io.write(w).data
        }

      for (r <- 0 until readPorts)
        io.read(r).data := Mux(io.read(r).en, storage(io.read(r).addr), zero)

      for (i <- 0 until depth)
        io.all(i) := storage(i)

      io.allValid := exposeAll.B

    case BufStorageBackend.Mem =>
      val storage = Mem(depth, gen.cloneType)

      for (w <- 0 until writePorts)
        when(io.write(w).en) {
          storage(io.write(w).addr) := io.write(w).data
        }

      for (r <- 0 until readPorts)
        io.read(r).data := Mux(io.read(r).en, storage(io.read(r).addr), zero)

      for (i <- 0 until depth)
        io.all(i) := zero

      io.allValid := false.B

    case BufStorageBackend.SyncMem =>
      val storage = SyncReadMem(depth, gen.cloneType, SyncReadMem.WriteFirst)

      for (w <- 0 until writePorts)
        when(io.write(w).en) {
          storage.write(io.write(w).addr, io.write(w).data)
        }

      for (r <- 0 until readPorts)
        io.read(r).data := storage.read(io.read(r).addr, io.read(r).en)

      for (i <- 0 until depth)
        io.all(i) := zero

      io.allValid := false.B
  }
}
