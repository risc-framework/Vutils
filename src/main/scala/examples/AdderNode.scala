package examples

import vutils._
import vutils.graph._
import chisel3._
import chisel3.util.Fill

class AdderNodeIO(width: Int) extends Bundle {
  val lhs = Input(UInt(width.W))
  val rhs = Input(UInt(width.W))
  val out = Output(UInt(width.W))
}

object AdderDims {
  val MODE = NodeDim("mode")
  val PIPE = NodeDim("pipe")
}

object AdderNodeMeta {
  val Type = NodeType("adder")
}

trait AdderModeImpl extends NodeDimensionImpl {
  override def nodeType: NodeType = AdderNodeMeta.Type
  override def dim: NodeDim       = AdderDims.MODE
  override def name: String       = value

  def add(lhs: UInt, rhs: UInt): UInt
}

trait AdderPipeImpl extends NodeDimensionImpl {
  override def nodeType: NodeType = AdderNodeMeta.Type
  override def dim: NodeDim       = AdderDims.PIPE
  override def name: String       = value

  def apply(x: UInt): UInt
}

object AdderModeFactory extends NodeDimensionRegistry[AdderModeImpl](AdderNodeMeta.Type, AdderDims.MODE)
object AdderPipeFactory extends NodeDimensionRegistry[AdderPipeImpl](AdderNodeMeta.Type, AdderDims.PIPE)

object WrapAdderMode extends RegisteredNodeUtils[AdderModeImpl] {
  override def utils: AdderModeImpl = new AdderModeImpl {
    override def value: String                   = "wrap"
    override def add(lhs: UInt, rhs: UInt): UInt = lhs +% rhs
  }

  override def registry: NodeRegistry[AdderModeImpl] = AdderModeFactory
}

object SaturatingAdderMode extends RegisteredNodeUtils[AdderModeImpl] {
  override def utils: AdderModeImpl = new AdderModeImpl {
    override def value: String = "sat"

    override def add(lhs: UInt, rhs: UInt): UInt = {
      val width = lhs.getWidth
      val sum   = lhs +& rhs
      val max   = Fill(width, 1.U(1.W))
      Mux(sum(width), max, sum(width - 1, 0))
    }
  }

  override def registry: NodeRegistry[AdderModeImpl] = AdderModeFactory
}

object CombAdderPipe extends RegisteredNodeUtils[AdderPipeImpl] {
  override def utils: AdderPipeImpl = new AdderPipeImpl {
    override def value: String        = "comb"
    override def apply(x: UInt): UInt = x
  }

  override def registry: NodeRegistry[AdderPipeImpl] = AdderPipeFactory
}

object Reg1AdderPipe extends RegisteredNodeUtils[AdderPipeImpl] {
  override def utils: AdderPipeImpl = new AdderPipeImpl {
    override def value: String        = "reg1"
    override def apply(x: UInt): UInt = RegNext(x)
  }

  override def registry: NodeRegistry[AdderPipeImpl] = AdderPipeFactory
}

object AdderNodeInit {
  val wrap = WrapAdderMode
  val sat  = SaturatingAdderMode
  val comb = CombAdderPipe
  val reg1 = Reg1AdderPipe
}

class AdderNode(config: NodeConfig) extends Node(new AdderNodeIO(config.optionOrElse[Int]("width", 32))) {
  override def nodeType: NodeType  = AdderNodeMeta.Type
  override def desiredName: String = s"adder_${config.selector.canonicalName}"

  private val modeImpl = AdderModeFactory.select(config)
  private val pipeImpl = AdderPipeFactory.select(config)
  private val sum      = modeImpl.add(io.lhs, io.rhs)

  io.out := pipeImpl(sum)
}

class AdderNodeExample extends Module {
  private val cfg = NodeConfig(
    selector = NodeSelector(
      AdderDims.MODE -> "sat",
      AdderDims.PIPE -> "reg1"
    ),
    options = Map("width" -> 32)
  )

  val io = IO(new Bundle {
    val lhs = Input(UInt(32.W))
    val rhs = Input(UInt(32.W))
    val out = Output(UInt(32.W))
  })

  private val adder = Module(new AdderNode(cfg))

  adder.io.lhs := io.lhs
  adder.io.rhs := io.rhs
  io.out       := adder.io.out
}

object AdderNodeExample extends App {
  AdderNodeInit

  DesignEmitter.emit(
    gen = new AdderNodeExample,
    filename = "adder_node_example",
    target = SystemVerilog,
    info = true,
    lowering = true,
  )
}
