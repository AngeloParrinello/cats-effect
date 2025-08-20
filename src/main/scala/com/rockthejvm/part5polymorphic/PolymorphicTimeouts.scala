package com.rockthejvm.part5polymorphic

import cats.effect.kernel.{Concurrent, Temporal}
import cats.effect.{IO, IOApp}
import cats.syntax.all.*

import scala.concurrent.duration.FiniteDuration
import com.rockthejvm.utils.general.*
import scala.concurrent.duration.*


object PolymorphicTimeouts extends IOApp.Simple {

  // Temporal - time blocking effects
  // abilities to sleep, flatMap, raiseError, uncancelable, etc.
  trait MyTemporal[F[_]] extends Concurrent[F]{
    def sleep(duration: FiniteDuration): F[Unit]
  }

  val temporalIo = Temporal[IO]
  val chainOfEffects = IO("Loading...").debug *> IO.sleep(1.second) *> IO("Done!").debug
  val chainOfEffectsV2 = temporalIo.pure("Loading...").debug *>
    temporalIo.sleep(1.second) *>
    temporalIo.pure("Done!").debug

  // Exercise: generalize this code
  //  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = {
  //    val timeout = IO.sleep(duration)
  //    IO.race(io, timeout).flatMap {
  //      case Left(v) => IO(v)
  //      case Right(_) => IO.raiseError(new RuntimeException(s"IO timed out after $duration"))
  //    }
  //  }
  def timeoutV2[F[_] : Temporal, A](fa: F[A], duration: FiniteDuration): F[A] =
    val temporal = summon[Temporal[F]]
    val timeout = temporal.sleep(duration)
    temporal.race(fa, timeout).flatMap {
      case Left(v) => temporal.pure(v)
      case Right(_) => temporal.raiseError(new RuntimeException(s"Effect timed out after $duration"))
    }


  def timeout[F[_], A](fa: F[A], duration: FiniteDuration)
                      (using temporal: Temporal[F]): F[A] =
    val timeout = temporal.sleep(duration)
    temporal.race(fa, timeout).flatMap {
      case Left(v) => temporal.pure(v)
      case Right(_) => temporal.raiseError(new RuntimeException(s"Effect timed out after $duration"))
    }

  override def run: IO[Unit] = ???

}
