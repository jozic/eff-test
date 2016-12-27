package com.daodecode

import org.atnos.eff.{/=, Eff, Safe, SafeEffect}
import org.atnos.eff.all.exception
import org.atnos.eff.eff.pure

import scala.reflect.ClassTag

package object efftest {

  implicit class SafeEffectOps2[R, A](private val e: Eff[R, A]) extends AnyVal {
    def whenException(onThrowable: PartialFunction[Throwable, Eff[R, A]])(implicit m: Safe /= R): Eff[R, A] = {
      catchException[A](identity[A], onThrowable)
    }

    def catchException[B](pureValue: A => B, onThrowable: PartialFunction[Throwable, Eff[R, B]])(implicit m: Safe /= R): Eff[R, B] = {
      SafeEffect.catchThrowable[R, A, B](e, pureValue, {
        case t if onThrowable.isDefinedAt(t) => onThrowable(t)
        case t => exception[R, B](t)
      })
    }

    def ignoreException2[E <: Throwable : ClassTag](implicit m: Safe /= R): Eff[R, Unit] =
      catchException[Unit](_ => (), {
        case t if implicitly[ClassTag[E]].runtimeClass.isInstance(t) => pure(())
      })

  }

}
