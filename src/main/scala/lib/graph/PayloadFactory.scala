package vutils.graph

import chisel3._
import scala.reflect.ClassTag

object PayloadFactory {
  def auto[C, P <: Data](ctx: C)(implicit tag: ClassTag[P]): P = {
    val cls    = tag.runtimeClass
    val ctxObj = ctx.asInstanceOf[AnyRef]

    val oneArgCtor = cls.getConstructors.find { ctor =>
      val params = ctor.getParameterTypes
      params.length == 1 && params.head.isInstance(ctxObj)
    }

    oneArgCtor match {
      case Some(ctor) =>
        ctor.newInstance(ctxObj).asInstanceOf[P]

      case None =>
        val zeroArgCtor = cls.getConstructors.find(_.getParameterCount == 0)

        zeroArgCtor match {
          case Some(ctor) =>
            ctor.newInstance().asInstanceOf[P]

          case None =>
            throw new IllegalArgumentException(
              s"PayloadFactory: cannot construct '${cls.getName}'. Expected zero-arg constructor or one-arg constructor compatible with ctx '${ctx.getClass.getName}'. Use inWith/outWith/etc."
            )
        }
    }
  }
}
