package vutils.buf

import chisel3._
import chisel3.util.log2Ceil

sealed trait BufStorageBackend {
  def readLatency: Int
  def supportsAllView: Boolean
}

object BufStorageBackend {
  case object Reg extends BufStorageBackend {
    override def readLatency: Int         = 0
    override def supportsAllView: Boolean = true
  }

  case object Mem extends BufStorageBackend {
    override def readLatency: Int         = 0
    override def supportsAllView: Boolean = false
  }

  case object SyncMem extends BufStorageBackend {
    override def readLatency: Int         = 1
    override def supportsAllView: Boolean = false
  }
}

sealed trait BufIndexSelectPolicy

object BufIndexSelectPolicy {
  case object LowIndex  extends BufIndexSelectPolicy
  case object HighIndex extends BufIndexSelectPolicy
}

sealed trait BufAgeSelectPolicy

object BufAgeSelectPolicy {
  case object Oldest   extends BufAgeSelectPolicy
  case object Youngest extends BufAgeSelectPolicy
}

object BufUtils {
  def addrWidth(depth: Int): Int = {
    require(depth > 0, "buffer depth must be positive")
    math.max(1, log2Ceil(depth))
  }

  def countWidth(depth: Int): Int = {
    require(depth > 0, "buffer depth must be positive")
    math.max(1, log2Ceil(depth + 1))
  }
}

class BufReadPort[T <: Data](private val gen: T, val depth: Int) extends Bundle {
  val en   = Input(Bool())
  val addr = Input(UInt(BufUtils.addrWidth(depth).W))
  val data = Output(gen.cloneType)
}

class BufWritePort[T <: Data](private val gen: T, val depth: Int) extends Bundle {
  val en   = Input(Bool())
  val addr = Input(UInt(BufUtils.addrWidth(depth).W))
  val data = Input(gen.cloneType)
}

class BufCamLookupPort[Entry <: Data, Key <: Data](private val entryGen: Entry, private val keyGen: Key, val depth: Int) extends Bundle {
  val en    = Input(Bool())
  val key   = Input(keyGen.cloneType)
  val hit   = Output(Bool())
  val index = Output(UInt(BufUtils.addrWidth(depth).W))
  val data  = Output(entryGen.cloneType)
  val hits  = Output(Vec(depth, Bool()))
}

class BufCamWritePort[T <: Data](private val gen: T, val depth: Int) extends Bundle {
  val en    = Input(Bool())
  val index = Input(UInt(BufUtils.addrWidth(depth).W))
  val valid = Input(Bool())
  val data  = Input(gen.cloneType)
}

class BufCamRemovePort(val depth: Int) extends Bundle {
  val en    = Input(Bool())
  val index = Input(UInt(BufUtils.addrWidth(depth).W))
}
