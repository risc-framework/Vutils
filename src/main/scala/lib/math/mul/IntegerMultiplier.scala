package vutils.math.mul

import chisel3._
import chisel3.util.{ Cat, Decoupled, log2Ceil, switch, is }
import scala.collection.mutable.ArrayBuffer

class IntegerMultiplierReq(val dw: Int) extends Bundle {
  val multiplicand = UInt(dw.W)
  val multiplier   = UInt(dw.W)

  val aSigned  = Bool()
  val bSigned  = Bool()
  val takeHigh = Bool()
}

class IntegerMultiplierResp(val dw: Int) extends Bundle {
  val result = UInt(dw.W)
}

class IntegerMultiplierIO(val dw: Int) extends Bundle {
  val in   = Flipped(Decoupled(new IntegerMultiplierReq(dw)))
  val out  = Decoupled(new IntegerMultiplierResp(dw))
  val kill = Input(Bool())
  val busy = Output(Bool())
}

private class FlushableQueue[T <: Data](gen: T, entries: Int) extends Module {
  require(entries > 0, "FlushableQueue entries must be > 0")

  val io = IO(new Bundle {
    val enq   = Flipped(Decoupled(gen))
    val deq   = Decoupled(gen)
    val flush = Input(Bool())
  })

  private val ptrWidth   = math.max(1, log2Ceil(entries))
  private val countWidth = log2Ceil(entries + 1)

  private val ram    = Reg(Vec(entries, gen))
  private val enqPtr = RegInit(0.U(ptrWidth.W))
  private val deqPtr = RegInit(0.U(ptrWidth.W))
  private val count  = RegInit(0.U(countWidth.W))

  private def wrapInc(ptr: UInt): UInt =
    Mux(ptr === (entries - 1).U, 0.U, ptr + 1.U)

  private val empty = count === 0.U
  private val full  = count === entries.U

  io.enq.ready := !full || io.deq.ready
  io.deq.valid := !empty
  io.deq.bits  := ram(deqPtr)

  private val enqFire = io.enq.fire
  private val deqFire = io.deq.fire

  when(io.flush) {
    enqPtr := 0.U
    deqPtr := 0.U
    count  := 0.U
  }.otherwise {
    when(enqFire) {
      ram(enqPtr) := io.enq.bits
    }

    when(enqFire) {
      enqPtr := wrapInc(enqPtr)
    }

    when(deqFire) {
      deqPtr := wrapInc(deqPtr)
    }

    switch(Cat(enqFire, deqFire)) {
      is("b10".U)(count := count + 1.U)
      is("b01".U)(count := count - 1.U)
      is("b11".U)(count := count)
      is("b00".U)(count := count)
    }
  }
}

class IntegerMultiplier(dw: Int, pipeline_stages: Int = 1) extends Module {
  require(dw > 0, "dw must be > 0")
  require(pipeline_stages >= 1, "pipeline_stages must be >= 1")

  override def desiredName: String =
    s"booth4_wallace_mul_${dw}_s$pipeline_stages"

  val io = IO(new IntegerMultiplierIO(dw))

  private val extWidth            = dw + 1
  private val productWidth        = 2 * dw
  private val wallaceTargetStages = pipeline_stages - 1

  private def fullAdder(a: UInt, b: UInt, c: UInt, width: Int): (UInt, UInt) = {
    val sum   = (a ^ b ^ c)(width - 1, 0)
    val carry = Cat((a & b) | (a & c) | (b & c), 0.U(1.W))(width - 1, 0)
    (sum, carry)
  }

  private def halfAdder(a: UInt, b: UInt, width: Int): (UInt, UInt) = {
    val sum   = (a ^ b)(width - 1, 0)
    val carry = Cat(a & b, 0.U(1.W))(width - 1, 0)
    (sum, carry)
  }

  private def wallaceLayerCount(inputCount: Int): Int = {
    var count  = inputCount
    var layers = 0

    while (count > 2) {
      val fullAdders  = count / 3
      val halfAdders  = (count % 3) / 2
      val passthrough = count  % 3 % 2

      count = fullAdders * 2 + halfAdders * 2 + passthrough
      layers += 1
    }

    layers
  }

  private def wallaceTreeReduce(inputs: Seq[UInt], width: Int): (UInt, UInt) = {
    var layer = inputs

    while (layer.length > 2) {
      val next = ArrayBuffer[UInt]()
      var i    = 0

      while (i < layer.length)
        if (i + 2 < layer.length) {
          val (sum, carry) = fullAdder(layer(i), layer(i + 1), layer(i + 2), width)
          next += sum
          next += carry
          i += 3
        } else if (i + 1 < layer.length) {
          val (sum, carry) = halfAdder(layer(i), layer(i + 1), width)
          next += sum
          next += carry
          i += 2
        } else {
          next += layer(i)
          i += 1
        }

      layer = next.toSeq
    }

    if (layer.length == 2) {
      (layer(0), layer(1))
    } else if (layer.nonEmpty) {
      (layer.head, 0.U(width.W))
    } else {
      (0.U(width.W), 0.U(width.W))
    }
  }

  private def wallaceTreeReducePipelined(
    inputs: Seq[UInt],
    stages: Int,
    width: Int
  ): (UInt, UInt, Int) = {
    val totalLayers  = wallaceLayerCount(inputs.length)
    val actualStages = stages.min(totalLayers)

    if (stages > totalLayers) {
      println(
        s"[WARN] IntegerMultiplier(dw=$dw): pipeline_stages=$pipeline_stages requests " +
          s"$stages Wallace register cuts, but only $totalLayers Wallace layers exist. " +
          s"Actual total latency is ${actualStages + 1} cycles."
      )
    }

    val cutPoints = (1 to actualStages)
      .map(i => (i * totalLayers) / (actualStages + 1))
      .filter(_ > 0)
      .toSet

    var layer      = inputs
    var layerIndex = 0

    while (layer.length > 2) {
      val next = ArrayBuffer[UInt]()
      var i    = 0

      while (i < layer.length)
        if (i + 2 < layer.length) {
          val (sum, carry) = fullAdder(layer(i), layer(i + 1), layer(i + 2), width)
          next += sum
          next += carry
          i += 3
        } else if (i + 1 < layer.length) {
          val (sum, carry) = halfAdder(layer(i), layer(i + 1), width)
          next += sum
          next += carry
          i += 2
        } else {
          next += layer(i)
          i += 1
        }

      layerIndex += 1

      layer = if (cutPoints.contains(layerIndex)) {
        next.map(RegNext(_)).toSeq
      } else {
        next.toSeq
      }
    }

    val missingStages = actualStages - cutPoints.size

    if (missingStages > 0) {
      layer = (0 until missingStages).foldLeft(layer) { case (current, _) =>
        current.map(RegNext(_))
      }
    }

    val (out0, out1) =
      if (layer.length == 2) {
        (layer(0), layer(1))
      } else if (layer.nonEmpty) {
        (layer.head, 0.U(width.W))
      } else {
        (0.U(width.W), 0.U(width.W))
      }

    (out0, out1, actualStages)
  }

  private val totalWallaceLayers  = wallaceLayerCount((extWidth + 1) / 2)
  private val actualWallaceStages = wallaceTargetStages.min(totalWallaceLayers)
  private val actualDelay         = actualWallaceStages + 1
  private val outputQueueDepth    = actualDelay + 2

  private val outQueue =
    Module(new FlushableQueue(new IntegerMultiplierResp(dw), outputQueueDepth))

  private val outstandingWidth = log2Ceil(outputQueueDepth + 1)
  private val outstanding      = RegInit(0.U(outstandingWidth.W))

  private val queueWillDeq = outQueue.io.deq.valid && io.out.ready && !io.kill
  private val hasCredit    = outstanding =/= outputQueueDepth.U || queueWillDeq

  io.in.ready := hasCredit && !io.kill

  private val inFire = io.in.fire

  private val aReg        = Reg(UInt(dw.W))
  private val bReg        = Reg(UInt(dw.W))
  private val aSignedReg  = Reg(Bool())
  private val bSignedReg  = Reg(Bool())
  private val takeHighReg = Reg(Bool())

  when(inFire) {
    aReg        := io.in.bits.multiplicand
    bReg        := io.in.bits.multiplier
    aSignedReg  := io.in.bits.aSigned
    bSignedReg  := io.in.bits.bSigned
    takeHighReg := io.in.bits.takeHigh
  }

  private val aExt =
    Cat(Mux(aSignedReg, aReg(dw - 1), 0.U(1.W)), aReg)

  private val bExt =
    Cat(Mux(bSignedReg, bReg(dw - 1), 0.U(1.W)), bReg)

  private val ppGen = Module(new PartialProductGen(extWidth, productWidth))
  ppGen.io.multiplicand       := aExt
  ppGen.io.multiplier         := bExt
  ppGen.io.multiplicandSigned := aSignedReg
  ppGen.io.multiplierSigned   := bSignedReg

  private val (carrySave0, carrySave1, actualTreeStages) =
    if (wallaceTargetStages > 0) {
      wallaceTreeReducePipelined(ppGen.io.partialProducts, wallaceTargetStages, productWidth)
    } else {
      val pair = wallaceTreeReduce(ppGen.io.partialProducts, productWidth)
      (pair._1, pair._2, 0)
    }

  private val latency = actualTreeStages + 1

  private val validPipe    = RegInit(VecInit(Seq.fill(latency)(false.B)))
  private val takeHighPipe =
    if (latency > 1) Some(RegInit(VecInit(Seq.fill(latency - 1)(false.B)))) else None

  when(io.kill) {
    validPipe.foreach(_ := false.B)
    takeHighPipe.foreach(_.foreach(_ := false.B))
  }.otherwise {
    validPipe(0)   := inFire
    for (i <- 1 until latency)
      validPipe(i) := validPipe(i - 1)

    takeHighPipe match {
      case Some(pipe) =>
        pipe(0)   := takeHighReg
        for (i <- 1 until pipe.length)
          pipe(i) := pipe(i - 1)

      case None =>
    }
  }

  private val takeHighAligned =
    takeHighPipe match {
      case Some(pipe) => pipe.last
      case None       => takeHighReg
    }

  private val fullResult     = carrySave0 + carrySave1
  private val selectedResult =
    Mux(takeHighAligned, fullResult(productWidth - 1, dw), fullResult(dw - 1, 0))

  outQueue.io.flush           := io.kill
  outQueue.io.enq.valid       := validPipe.last && !io.kill
  outQueue.io.enq.bits.result := selectedResult

  when(outQueue.io.enq.valid) {
    assert(outQueue.io.enq.ready, "IntegerMultiplier output queue overflow")
  }

  io.out.valid          := outQueue.io.deq.valid && !io.kill
  io.out.bits           := outQueue.io.deq.bits
  outQueue.io.deq.ready := io.out.ready && !io.kill

  when(io.kill) {
    outstanding := 0.U
  }.otherwise {
    val inc = inFire.asUInt
    val dec = io.out.fire.asUInt
    outstanding := outstanding + inc - dec
  }

  io.busy := outstanding =/= 0.U
}
