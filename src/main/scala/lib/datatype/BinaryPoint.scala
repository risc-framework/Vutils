package vutils.datatype

object BinaryPoint {
  def apply(value: Int): BinaryPoint = KnownBinaryPoint(value)
  def apply(): BinaryPoint           = UnknownBinaryPoint

  object Implicits {
    implicit class IntToBinaryPoint(private val value: Int) extends AnyVal {
      def BP: BinaryPoint = BinaryPoint(value)
    }
  }
}

sealed abstract class BinaryPoint extends Serializable {
  type BP = Int

  def known: Boolean
  def get: BP
  def option: Option[Int]

  final def unknown: Boolean = !known

  final def requireKnown(message: String = "unknown binary point"): Int =
    option.getOrElse(throw new NoSuchElementException(message))

  final def min(that: BinaryPoint): BinaryPoint = op(that, _ min _)
  final def max(that: BinaryPoint): BinaryPoint = op(that, _ max _)
  final def +(that: BinaryPoint): BinaryPoint   = op(that, _ + _)

  final def +(that: Int): BinaryPoint =
    if (known) BinaryPoint(get + that) else UnknownBinaryPoint

  final def -(that: Int): BinaryPoint =
    if (known) BinaryPoint(get - that) else UnknownBinaryPoint

  final def unsignedShiftRight(that: Int): BinaryPoint =
    if (known) BinaryPoint(0.max(get - that)) else UnknownBinaryPoint

  final def signedShiftRight(that: Int): BinaryPoint =
    if (known) BinaryPoint(1.max(get - that)) else UnknownBinaryPoint

  final def dynamicShiftLeft(that: BinaryPoint): BinaryPoint =
    op(that, (a, b) => a + (1 << b) - 1)

  protected def op(that: BinaryPoint, f: (BP, BP) => BP): BinaryPoint
}

case object UnknownBinaryPoint extends BinaryPoint {
  override def known: Boolean      = false
  override def get: Int            = throw new NoSuchElementException("unknown binary point")
  override def option: Option[Int] = None

  override protected def op(that: BinaryPoint, f: (BP, BP) => BP): BinaryPoint =
    UnknownBinaryPoint

  override def toString: String = ""
}

final class KnownBinaryPoint private[datatype] (val value: Int) extends BinaryPoint {
  require(value >= 0, s"BinaryPoint must be non-negative, got $value")

  override def known: Boolean      = true
  override def get: Int            = value
  override def option: Option[Int] = Some(value)

  override protected def op(that: BinaryPoint, f: (BP, BP) => BP): BinaryPoint =
    that match {
      case that: KnownBinaryPoint => KnownBinaryPoint(f(value, that.value))
      case _                      => UnknownBinaryPoint
    }

  override def equals(that: Any): Boolean =
    that match {
      case that: KnownBinaryPoint => value == that.value
      case _                      => false
    }

  override def hashCode: Int = value.hashCode

  override def toString: String = s"<$value>"
}

object KnownBinaryPoint {
  private val maxCached = 1024
  private val cache     = new Array[KnownBinaryPoint](maxCached + 1)

  def apply(value: Int): KnownBinaryPoint = {
    require(value >= 0, s"BinaryPoint must be non-negative, got $value")

    if (value <= maxCached) {
      var cached = cache(value)
      if (cached eq null) {
        cached = new KnownBinaryPoint(value)
        cache(value) = cached
      }
      cached
    } else {
      new KnownBinaryPoint(value)
    }
  }

  def unapply(value: KnownBinaryPoint): Option[Int] = Some(value.value)
}
