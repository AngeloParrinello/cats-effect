package com.rockthejvm.utils.general

import cats.Functor
import cats.effect.MonadCancel
import cats.syntax.functor.*

import scala.concurrent.duration.FiniteDuration

extension [F[_], A](fa: F[A]) {
  def debug(using functor: Functor[F]): F[A] =
    fa.map {
      a =>
        val threadName = Thread.currentThread().getName
        println(s"[$threadName] $a")
        a
    }
}

def unsafeSleep[F[_], E](duration: FiniteDuration)(implicit
                                                   monadCancel: MonadCancel[F, E]
): F[Unit] = {
  monadCancel.pure {
    Thread.sleep(duration.toMillis) // not semantic blocking
  }
}
