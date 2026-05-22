package vutils

import chisel3.Width

package object datatype {
  implicit class IntToBinaryPointSyntax(private val value: Int) extends AnyVal {
    def BP: BinaryPoint = BinaryPoint(value)
  }

  implicit class IntToFixedPointLiteralSyntax(private val value: Int) extends AnyVal {
    def F(binaryPoint: BinaryPoint): FixedPoint =
      FixedPoint.fromInt(BigInt(value), binaryPoint)

    def F(width: Width, binaryPoint: BinaryPoint): FixedPoint =
      FixedPoint.fromInt(BigInt(value), width, binaryPoint)
  }

  implicit class BigIntToFixedPointLiteralSyntax(private val value: BigInt) extends AnyVal {
    def F(binaryPoint: BinaryPoint): FixedPoint =
      FixedPoint.fromInt(value, binaryPoint)

    def F(width: Width, binaryPoint: BinaryPoint): FixedPoint =
      FixedPoint.fromInt(value, width, binaryPoint)
  }

  implicit class DoubleToFixedPointLiteralSyntax(private val value: Double) extends AnyVal {
    def F(binaryPoint: BinaryPoint): FixedPoint =
      FixedPoint.fromDouble(value, binaryPoint)

    def F(width: Width, binaryPoint: BinaryPoint): FixedPoint =
      FixedPoint.fromDouble(value, width, binaryPoint)
  }

  implicit class BigDecimalToFixedPointLiteralSyntax(private val value: BigDecimal) extends AnyVal {
    def F(binaryPoint: BinaryPoint): FixedPoint =
      FixedPoint.fromBigDecimal(value, binaryPoint)

    def F(width: Width, binaryPoint: BinaryPoint): FixedPoint =
      FixedPoint.fromBigDecimal(value, width, binaryPoint)
  }
}
