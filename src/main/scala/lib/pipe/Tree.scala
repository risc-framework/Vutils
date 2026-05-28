package vutils.pipe

import vutils.algebra.group.SemiGroup
import chisel3._
import chisel3.util.ShiftRegister

trait Tree[T <: Data] extends SemiGroup[T] {
  protected def treeReduce(items: Seq[T]): T =
    items.length match {
      case 0 =>
        throw new IllegalArgumentException("treeReduce on empty array!")

      case 1 =>
        items.head

      case _ =>
        val mid = items.length / 2
        val lhs = treeReduce(items.take(mid))
        val rhs = treeReduce(items.drop(mid))

        op(lhs, rhs)
    }

  protected def treeReducePipe(items: Seq[T], interval: Int = 1): T = {
    require(interval >= 0, "treeReducePipe: interval must be >= 0")

    items.length match {
      case 0 =>
        throw new IllegalArgumentException("treeReducePipe on empty array!")

      case 1 =>
        items.head

      case _ =>
        val mid = items.length / 2
        val lhs = treeReducePipe(items.take(mid), interval)
        val rhs = treeReducePipe(items.drop(mid), interval)
        val sum = op(lhs, rhs)

        ShiftRegister(sum, interval)
    }
  }
}
