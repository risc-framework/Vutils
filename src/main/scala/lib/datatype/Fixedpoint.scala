package vutils.datatype

import chisel3._
import chisel3.experimental.OpaqueType
import chisel3.experimental.SourceInfo

import scala.collection.immutable.SeqMap
import scala.math.BigDecimal.RoundingMode

sealed trait FixedPointRounding
object FixedPointRounding {
  case object Truncate              extends FixedPointRounding
  case object RoundTowardZero       extends FixedPointRounding
  case object RoundHalfAwayFromZero extends FixedPointRounding
}

sealed trait FixedPointOverflow
object FixedPointOverflow {
  case object Wrap     extends FixedPointOverflow
  case object Saturate extends FixedPointOverflow
}

object FixedPoint extends NumObject {
  import FixedPointRounding._

  def apply(): FixedPoint =
    new FixedPoint(UnknownWidth, BinaryPoint())

  def apply(width: Width, binaryPoint: BinaryPoint): FixedPoint =
    new FixedPoint(width, binaryPoint)

  def apply(width: Width, binaryPoint: BinaryPoint, value: Int)(implicit sourceInfo: SourceInfo): FixedPoint =
    fromInt(BigInt(value), width, binaryPoint)

  def apply(width: Width, binaryPoint: BinaryPoint, value: BigInt)(implicit sourceInfo: SourceInfo): FixedPoint =
    fromInt(value, width, binaryPoint)

  def apply(width: Width, binaryPoint: BinaryPoint, value: Double)(implicit sourceInfo: SourceInfo): FixedPoint =
    fromDouble(value, width, binaryPoint)

  def fromRaw(raw: BigInt, binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo): FixedPoint =
    fromRaw(raw, UnknownWidth, binaryPoint)

  def fromRaw(raw: BigInt, width: Width, binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo): FixedPoint = {
    binaryPoint.requireKnown("FixedPoint literal requires known binary point")

    val literalWidth =
      if (width.known) {
        require(
          fitsSigned(raw, width.get),
          s"raw literal $raw does not fit in signed width ${width.get}"
        )
        width
      } else {
        signedWidthOf(raw).W
      }

    fromData(binaryPoint, raw.S(literalWidth), Some(literalWidth))
  }

  def fromInt(value: BigInt, binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo): FixedPoint =
    fromInt(value, UnknownWidth, binaryPoint)

  def fromInt(value: BigInt, width: Width, binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo): FixedPoint = {
    val bp  = binaryPoint.requireKnown("FixedPoint integer literal requires known binary point")
    val raw = value << bp
    fromRaw(raw, width, binaryPoint)
  }

  def fromDouble(value: Double, binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo): FixedPoint =
    fromDouble(value, UnknownWidth, binaryPoint)

  def fromDouble(value: Double, width: Width, binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo): FixedPoint = {
    require(!value.isNaN && !value.isInfinity, s"invalid FixedPoint Double literal: $value")
    fromBigDecimal(BigDecimal.decimal(value), width, binaryPoint)
  }

  def fromBigDecimal(value: BigDecimal, binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo): FixedPoint =
    fromBigDecimal(value, UnknownWidth, binaryPoint)

  def fromBigDecimal(
    value: BigDecimal,
    width: Width,
    binaryPoint: BinaryPoint
  )(implicit sourceInfo: SourceInfo): FixedPoint = {
    val raw = decimalToRaw(value, binaryPoint, RoundingMode.HALF_UP)
    fromRaw(raw, width, binaryPoint)
  }

  def zero(width: Width, binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo): FixedPoint =
    fromRaw(0, width, binaryPoint)

  def one(width: Width, binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo): FixedPoint =
    fromInt(1, width, binaryPoint)

  def minValue(width: Width, binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo): FixedPoint = {
    require(width.known, "FixedPoint.minValue requires known width")
    fromRaw(minRaw(width.get), width, binaryPoint)
  }

  def maxValue(width: Width, binaryPoint: BinaryPoint)(implicit sourceInfo: SourceInfo): FixedPoint = {
    require(width.known, "FixedPoint.maxValue requires known width")
    fromRaw(maxRaw(width.get), width, binaryPoint)
  }

  private[datatype] def fromData(
    binaryPoint: BinaryPoint,
    value: Data,
    width: Option[Width] = None
  )(implicit sourceInfo: SourceInfo): FixedPoint = {
    val out = Wire(FixedPoint(width.getOrElse(recreateWidth(value)), binaryPoint))
    out.data := value.asTypeOf(out.data)
    out
  }

  private[datatype] def recreateWidth(value: Data): Width =
    value.widthOption.fold[Width](UnknownWidth)(_.W)

  private[datatype] def dataAligned(
    values: Iterable[FixedPoint]
  )(implicit sourceInfo: SourceInfo): Seq[FixedPoint] = {
    require(values.nonEmpty, "cannot align empty FixedPoint sequence")

    values.foreach(_.requireKnownBP())
    val maxBP = values.map(_.binaryPoint).reduce(_.max(_))

    val maxWidth = values
      .map { value =>
        recreateWidth(value.data) + (maxBP.get - value.binaryPoint.get)
      }
      .reduce(_.max(_))

    values.map { value =>
      fromData(
        maxBP,
        rawAtBinaryPoint(value, maxBP, Truncate),
        Some(maxWidth)
      )
    }.toSeq
  }

  private[datatype] def dataAlignedPair(
    a: FixedPoint,
    b: FixedPoint
  )(implicit sourceInfo: SourceInfo): (FixedPoint, FixedPoint) = {
    val aligned = dataAligned(Seq(a, b))
    (aligned(0), aligned(1))
  }

  private[datatype] def rawAtBinaryPoint(
    value: FixedPoint,
    targetBinaryPoint: BinaryPoint,
    rounding: FixedPointRounding
  )(implicit sourceInfo: SourceInfo): SInt = {
    value.requireKnownBP()
    targetBinaryPoint.requireKnown("target binary point must be known")

    val diff = targetBinaryPoint.get - value.binaryPoint.get

    if (diff > 0) {
      value.data << diff
    } else if (diff < 0) {
      shiftRight(value.data, -diff, rounding)
    } else {
      value.data
    }
  }

  private[datatype] def shiftRight(
    value: SInt,
    amount: Int,
    rounding: FixedPointRounding
  )(implicit sourceInfo: SourceInfo): SInt = {
    require(amount >= 0, s"negative shift amount $amount")

    if (amount == 0) {
      value
    } else {
      rounding match {
        case Truncate =>
          value >> amount

        case RoundTowardZero =>
          val bias = ((BigInt(1) << amount) - 1).S
          (value + Mux(value < 0.S, bias, 0.S)) >> amount

        case RoundHalfAwayFromZero =>
          val half    = (BigInt(1) << (amount - 1)).S
          val abs     = Mux(value < 0.S, -value, value)
          val rounded = (abs + half) >> amount
          Mux(value < 0.S, -rounded, rounded)
      }
    }
  }

  private def decimalToRaw(
    value: BigDecimal,
    binaryPoint: BinaryPoint,
    roundingMode: RoundingMode.Value
  ): BigInt = {
    val bp    = binaryPoint.requireKnown("FixedPoint decimal literal requires known binary point")
    val scale = BigDecimal(2).pow(bp)
    (value * scale).setScale(0, roundingMode).toBigIntExact.get
  }

  private def signedWidthOf(value: BigInt): Int =
    if (value >= 0) {
      value.bitLength.max(0) + 1
    } else {
      ((-value) - 1).bitLength + 1
    }

  private def fitsSigned(value: BigInt, width: Int): Boolean =
    width > 0 && minRaw(width) <= value && value <= maxRaw(width)

  private[datatype] def minRaw(width: Int): BigInt = {
    require(width > 0, s"FixedPoint signed width must be positive, got $width")
    -(BigInt(1) << (width - 1))
  }

  private[datatype] def maxRaw(width: Int): BigInt = {
    require(width > 0, s"FixedPoint signed width must be positive, got $width")
    (BigInt(1) << (width - 1)) - 1
  }
}

sealed class FixedPoint private[datatype] (
  private val _fixedWidth: Width,
  private var _binaryPoint: BinaryPoint
) extends Record
    with OpaqueType
    with Num[FixedPoint] {
  import FixedPointOverflow._
  import FixedPointRounding._

  if (_fixedWidth.known) {
    require(_fixedWidth.get > 0, s"FixedPoint width must be positive, got ${_fixedWidth.get}")
  }
  if (_binaryPoint.known) {
    require(_binaryPoint.get >= 0, s"FixedPoint binary point must be non-negative, got ${_binaryPoint.get}")
  }

  private[datatype] val data: SInt = SInt(_fixedWidth)

  override val elements: SeqMap[String, SInt] =
    SeqMap("" -> data)

  def fixedWidth: Width = _fixedWidth

  def binaryPoint: BinaryPoint = _binaryPoint

  private[datatype] def requireKnownBP(
    message: String = "unknown binary point is not supported for this FixedPoint operation"
  ): Unit =
    if (!binaryPoint.known) {
      throw new ChiselException(message)
    }

  override def typeName: String =
    s"FixedPoint$fixedWidth$binaryPoint"

  private def literal(value: BigDecimal): FixedPoint = {
    requireKnownBP("cannot create FixedPoint literal from value with unknown binary point")
    FixedPoint.fromBigDecimal(value, UnknownWidth, binaryPoint)
  }

  private def additiveOp(
    that: FixedPoint,
    op: (SInt, SInt) => SInt
  )(implicit sourceInfo: SourceInfo): FixedPoint = {
    val (a, b) = FixedPoint.dataAlignedPair(this, that)
    FixedPoint.fromData(a.binaryPoint, op(a.data, b.data))
  }

  private def comparativeOp(
    that: FixedPoint,
    op: (SInt, SInt) => Bool
  )(implicit sourceInfo: SourceInfo): Bool = {
    val (a, b) = FixedPoint.dataAlignedPair(this, that)
    op(a.data, b.data)
  }

  override def do_+(that: FixedPoint)(implicit sourceInfo: SourceInfo): FixedPoint =
    additiveOp(that, _ + _)

  override def do_-(that: FixedPoint)(implicit sourceInfo: SourceInfo): FixedPoint =
    additiveOp(that, _ - _)

  override def do_*(that: FixedPoint)(implicit sourceInfo: SourceInfo): FixedPoint = {
    requireKnownBP()
    that.requireKnownBP()

    FixedPoint.fromData(
      binaryPoint + that.binaryPoint,
      data * that.data
    )
  }

  override def do_/(that: FixedPoint)(implicit sourceInfo: SourceInfo): FixedPoint =
    div(that, binaryPoint)

  override def do_%(that: FixedPoint)(implicit sourceInfo: SourceInfo): FixedPoint = {
    val (a, b) = FixedPoint.dataAlignedPair(this, that)
    FixedPoint.fromData(a.binaryPoint, a.data % b.data)
  }

  override def do_<(that: FixedPoint)(implicit sourceInfo: SourceInfo): Bool =
    comparativeOp(that, _ < _)

  override def do_<=(that: FixedPoint)(implicit sourceInfo: SourceInfo): Bool =
    comparativeOp(that, _ <= _)

  override def do_>(that: FixedPoint)(implicit sourceInfo: SourceInfo): Bool =
    comparativeOp(that, _ > _)

  override def do_>=(that: FixedPoint)(implicit sourceInfo: SourceInfo): Bool =
    comparativeOp(that, _ >= _)

  override def do_abs(implicit sourceInfo: SourceInfo): FixedPoint =
    FixedPoint.fromData(binaryPoint, data.abs)

  def do_unary_-(implicit sourceInfo: SourceInfo): FixedPoint =
    FixedPoint.fromData(binaryPoint, -data)

  def unary_-(implicit sourceInfo: SourceInfo): FixedPoint =
    do_unary_-

  def +%(that: FixedPoint)(implicit sourceInfo: SourceInfo): FixedPoint =
    additiveOp(that, _ +% _)

  def +&(that: FixedPoint)(implicit sourceInfo: SourceInfo): FixedPoint =
    additiveOp(that, _ +& _)

  def -%(that: FixedPoint)(implicit sourceInfo: SourceInfo): FixedPoint =
    additiveOp(that, _ -% _)

  def -&(that: FixedPoint)(implicit sourceInfo: SourceInfo): FixedPoint =
    additiveOp(that, _ -& _)

  def +(that: Int)(implicit sourceInfo: SourceInfo): FixedPoint =
    this + literal(BigDecimal(that))

  def +(that: BigInt)(implicit sourceInfo: SourceInfo): FixedPoint =
    this + literal(BigDecimal(that))

  def +(that: Double)(implicit sourceInfo: SourceInfo): FixedPoint =
    this + literal(BigDecimal.decimal(that))

  def -(that: Int)(implicit sourceInfo: SourceInfo): FixedPoint =
    this - literal(BigDecimal(that))

  def -(that: BigInt)(implicit sourceInfo: SourceInfo): FixedPoint =
    this - literal(BigDecimal(that))

  def -(that: Double)(implicit sourceInfo: SourceInfo): FixedPoint =
    this - literal(BigDecimal.decimal(that))

  def *(that: Int)(implicit sourceInfo: SourceInfo): FixedPoint =
    this * literal(BigDecimal(that))

  def *(that: BigInt)(implicit sourceInfo: SourceInfo): FixedPoint =
    this * literal(BigDecimal(that))

  def *(that: Double)(implicit sourceInfo: SourceInfo): FixedPoint =
    this * literal(BigDecimal.decimal(that))

  def /(that: Int)(implicit sourceInfo: SourceInfo): FixedPoint =
    this / literal(BigDecimal(that))

  def /(that: BigInt)(implicit sourceInfo: SourceInfo): FixedPoint =
    this / literal(BigDecimal(that))

  def /(that: Double)(implicit sourceInfo: SourceInfo): FixedPoint =
    this / literal(BigDecimal.decimal(that))

  def ===(that: FixedPoint)(implicit sourceInfo: SourceInfo): Bool =
    comparativeOp(that, _ === _)

  def =/=(that: FixedPoint)(implicit sourceInfo: SourceInfo): Bool =
    comparativeOp(that, _ =/= _)

  def ===(that: Int)(implicit sourceInfo: SourceInfo): Bool =
    this === literal(BigDecimal(that))

  def ===(that: Double)(implicit sourceInfo: SourceInfo): Bool =
    this === literal(BigDecimal.decimal(that))

  def =/=(that: Int)(implicit sourceInfo: SourceInfo): Bool =
    this =/= literal(BigDecimal(that))

  def =/=(that: Double)(implicit sourceInfo: SourceInfo): Bool =
    this =/= literal(BigDecimal.decimal(that))

  def <(that: Int)(implicit sourceInfo: SourceInfo): Bool =
    this < literal(BigDecimal(that))

  def <(that: Double)(implicit sourceInfo: SourceInfo): Bool =
    this < literal(BigDecimal.decimal(that))

  def <=(that: Int)(implicit sourceInfo: SourceInfo): Bool =
    this <= literal(BigDecimal(that))

  def <=(that: Double)(implicit sourceInfo: SourceInfo): Bool =
    this <= literal(BigDecimal.decimal(that))

  def >(that: Int)(implicit sourceInfo: SourceInfo): Bool =
    this > literal(BigDecimal(that))

  def >(that: Double)(implicit sourceInfo: SourceInfo): Bool =
    this > literal(BigDecimal.decimal(that))

  def >=(that: Int)(implicit sourceInfo: SourceInfo): Bool =
    this >= literal(BigDecimal(that))

  def >=(that: Double)(implicit sourceInfo: SourceInfo): Bool =
    this >= literal(BigDecimal.decimal(that))

  def <<(that: Int)(implicit sourceInfo: SourceInfo): FixedPoint =
    FixedPoint.fromData(binaryPoint, data << that)

  def <<(that: UInt)(implicit sourceInfo: SourceInfo): FixedPoint =
    FixedPoint.fromData(binaryPoint, data << that)

  def >>(that: Int)(implicit sourceInfo: SourceInfo): FixedPoint =
    FixedPoint.fromData(binaryPoint, data >> that)

  def >>(that: UInt)(implicit sourceInfo: SourceInfo): FixedPoint =
    FixedPoint.fromData(binaryPoint, data >> that)

  def div(
    that: FixedPoint,
    outBinaryPoint: BinaryPoint
  )(implicit sourceInfo: SourceInfo): FixedPoint = {
    requireKnownBP()
    that.requireKnownBP()
    outBinaryPoint.requireKnown("division output binary point must be known")

    val shift = outBinaryPoint.get + that.binaryPoint.get - binaryPoint.get

    val numerator =
      if (shift >= 0) data << shift else data

    val denominator =
      if (shift >= 0) that.data else that.data << -shift

    FixedPoint.fromData(outBinaryPoint, numerator / denominator)
  }

  def withBinaryPoint(
    that: BinaryPoint
  )(implicit sourceInfo: SourceInfo): FixedPoint =
    FixedPoint.fromData(that, data, Some(fixedWidth))

  def asFixedPoint(
    that: BinaryPoint
  )(implicit sourceInfo: SourceInfo): FixedPoint =
    withBinaryPoint(that)

  def setBinaryPoint(
    that: Int,
    rounding: FixedPointRounding = Truncate
  )(implicit sourceInfo: SourceInfo): FixedPoint =
    setBinaryPoint(BinaryPoint(that), rounding)

  def setBinaryPoint(
    that: BinaryPoint,
    rounding: FixedPointRounding
  )(implicit sourceInfo: SourceInfo): FixedPoint =
    FixedPoint.fromData(
      that,
      FixedPoint.rawAtBinaryPoint(this, that, rounding)
    )

  def cast(
    width: Width,
    binaryPoint: BinaryPoint,
    rounding: FixedPointRounding = Truncate,
    overflow: FixedPointOverflow = Wrap
  )(implicit sourceInfo: SourceInfo): FixedPoint = {
    binaryPoint.requireKnown("cast target binary point must be known")
    setBinaryPoint(binaryPoint, rounding).resize(width, overflow)
  }

  def resize(
    width: Width,
    overflow: FixedPointOverflow = Wrap
  )(implicit sourceInfo: SourceInfo): FixedPoint =
    overflow match {
      case Wrap =>
        FixedPoint.fromData(binaryPoint, data, Some(width))

      case Saturate =>
        saturate(width)
    }

  def saturate(width: Width)(implicit sourceInfo: SourceInfo): FixedPoint = {
    require(width.known, "FixedPoint.saturate requires known output width")

    val outWidth = width.get
    val min      = FixedPoint.minRaw(outWidth).S(width)
    val max      = FixedPoint.maxRaw(outWidth).S(width)

    val clipped = Mux(
      data > max,
      max,
      Mux(data < min, min, data)
    )

    FixedPoint.fromData(binaryPoint, clipped, Some(width))
  }

  def floor(implicit sourceInfo: SourceInfo): FixedPoint = {
    requireKnownBP()

    val bp = binaryPoint.get
    if (bp == 0) {
      this
    } else {
      FixedPoint.fromData(
        binaryPoint,
        (data >> bp) << bp,
        Some(fixedWidth)
      )
    }
  }

  def ceil(implicit sourceInfo: SourceInfo): FixedPoint = {
    requireKnownBP()

    val bp = binaryPoint.get
    if (bp == 0) {
      this
    } else {
      val almostOne = ((BigInt(1) << bp) - 1).S
      FixedPoint.fromData(
        binaryPoint,
        ((data + almostOne) >> bp) << bp,
        Some(fixedWidth)
      )
    }
  }

  def round(implicit sourceInfo: SourceInfo): FixedPoint = {
    requireKnownBP()

    val bp = binaryPoint.get
    if (bp == 0) {
      this
    } else {
      setBinaryPoint(0, RoundHalfAwayFromZero)
        .setBinaryPoint(bp, Truncate)
        .resize(fixedWidth, Wrap)
    }
  }

  def trunc(implicit sourceInfo: SourceInfo): FixedPoint = {
    requireKnownBP()

    val bp = binaryPoint.get
    if (bp == 0) {
      this
    } else {
      setBinaryPoint(0, RoundTowardZero)
        .setBinaryPoint(bp, Truncate)
        .resize(fixedWidth, Wrap)
    }
  }

  def integer(implicit sourceInfo: SourceInfo): SInt = {
    requireKnownBP()
    data >> binaryPoint.get
  }

  def fractional(implicit sourceInfo: SourceInfo): UInt = {
    requireKnownBP()

    val bp = binaryPoint.get
    if (bp == 0) {
      0.U(0.W)
    } else {
      data.asUInt(bp - 1, 0)
    }
  }

  def isNegative: Bool = data < 0.S

  def isZero: Bool = data === 0.S

  def raw: SInt = data

  def rawUInt: UInt = data.asUInt

  final def asSInt: SInt = data.asSInt

  def apply(idx: Int)(implicit sourceInfo: SourceInfo): Bool =
    data(idx)

  def apply(idx: UInt)(implicit sourceInfo: SourceInfo): Bool =
    data(idx)

  def apply(high: Int, low: Int)(implicit sourceInfo: SourceInfo): UInt =
    data(high, low)

  private def connectOp(
    that: Data,
    connect: (Data, Data) => Unit
  )(implicit sourceInfo: SourceInfo): Unit =
    that match {
      case that: FixedPoint =>
        if (binaryPoint.known) {
          that.requireKnownBP("cannot connect unknown-binary-point FixedPoint into known-binary-point sink")
          connect(data, that.setBinaryPoint(binaryPoint.get).data)
        } else {
          if (that.binaryPoint.known) {
            _binaryPoint = BinaryPoint(that.binaryPoint.get)
          }
          connect(data, that.data)
        }

      case that @ DontCare =>
        connect(data, that)

      case _ =>
        throw new ChiselException(s"cannot connect FixedPoint and $that")
    }

  override def connect(that: Data)(implicit sourceInfo: SourceInfo): Unit =
    connectOp(that, _ := _)

  override def bulkConnect(that: Data)(implicit sourceInfo: SourceInfo): Unit =
    connectOp(that, _ <> _)

  override protected def _fromUInt(that: UInt)(implicit sourceInfo: SourceInfo): Data = {
    val out = Wire(this.cloneType)
    out.data := that.asTypeOf(out.data)
    out
  }

  def rawWidthOption: Option[Int] = data.widthOption

  def rawWidthKnown: Boolean = data.widthKnown

  override def litOption: Option[BigInt] = data.litOption

  override def litValue: BigInt = data.litValue

  def litToBigDecimalOption: Option[BigDecimal] =
    for {
      raw <- litOption
      bp  <- binaryPoint.option
    } yield BigDecimal(raw) / BigDecimal(2).pow(bp)

  def litToDoubleOption: Option[Double] =
    litToBigDecimalOption.map(_.toDouble)

  override def toString: String =
    litToBigDecimalOption match {
      case Some(value) => s"FixedPoint$fixedWidth$binaryPoint($value)"
      case None        => s"FixedPoint$fixedWidth$binaryPoint"
    }
}
