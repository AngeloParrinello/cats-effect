package com.rockthejvm.part5polymorphic

import cats.effect.*
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}

object PolymorphicFibers extends IOApp.Simple {


  // When you see Gen, think "generic" in the sense of "generic over the error type"
  // we have pure, map, flatMap, handleErrorWith, raiseError, uncancelable, canceled, start, etc.
  trait MyGenSpawn[F[_], E] extends MonadCancel[F, E] {
    def start[A](fa: F[A]): F[Fiber[F, E, A]] // creates a fiber!

    def never[A]: F[A] // a fiber that never completes, a forever suspended effect

    def cede: F[Unit] // yield the current fiber, allowing other fibers to run

    def racePair[A, B](fa: F[A], fb: F[B]): F[Either[(Outcome[F, E, A], Fiber[F, E, B]), (Fiber[F, E, A], Outcome[F, E, B])]]
  }

  // Spawn = create fibers for any effect
  trait MySpawn[F[_]] extends MyGenSpawn[F, Throwable] {
    def start[A](fa: F[A]): F[Fiber[F, Throwable, A]] // creates a fiber!

    def never[A]: F[A] // a fiber that never completes, a forever suspended effect

    def cede: F[Unit] // yield the current fiber, allowing other fibers to run
  }

  val mol = IO(42)
  val fiber: IO[FiberIO[Int]] = mol.start


  val spawnIO = Spawn[IO] // fetch the Spawn type class for IO

  def runOnSomeThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] =
    // we can use the Spawn type class to run the IO on a different thread
    spawnIO.start(io).flatMap(_.join) // join blocks until the fiber completes

  // generalize a bit..

  import cats.effect.unsafe.implicits.global
  import cats.syntax.flatMap.*

  def effectOnSomeThread[F[_], A](fa: F[A])(using spawn: Spawn[F]): F[Outcome[F, Throwable, A]] = {
    spawn.start(fa).flatMap(_.join) // join blocks until the fiber completes
  }

  // with the extension method syntax

  import cats.effect.syntax.spawn.*

  def effectOnSomeThreadV2[F[_], A](fa: F[A])(using spawn: Spawn[F]): F[Outcome[F, Throwable, A]] = {
    fa.start.flatMap(_.join) // join blocks until the fiber completes
  }

  val molOnFiber = effectOnSomeThread(mol) // run the IO on a fiber


  // Exercise generalize the following code
  /*
    def simpleRace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] = {
    IO.racePair(ioa, iob).flatMap {
      case Left((outputA, fiberB)) => outputA match {
        case Succeeded(effectA) => fiberB.cancel >> effectA.map(a => Left(a))
        case Errored(e) => fiberB.cancel >> IO.raiseError(e)
        case Canceled() => fiberB.join.flatMap {
          case Succeeded(effectB) => effectB.map(b => Right(b))
          case Errored(e) => IO.raiseError(e)
          case Canceled() => IO.raiseError(new RuntimeException("Both fibers were canceled"))
        }
      }
      case Right((fiberA, outputB)) => outputB match {
        case Succeeded(effectB) => fiberA.cancel >> effectB.map(b => Right(b))
        case Errored(e) => fiberA.cancel >> IO.raiseError(e)
        case Canceled() => fiberA.join.flatMap {
          case Succeeded(effectA) => effectA.map(a => Left(a))
          case Errored(e) => IO.raiseError(e)
          case Canceled() => IO.raiseError(new RuntimeException("Both fibers were canceled"))
        }
      }
    }

   */

  import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
  import cats.effect.syntax.all.*
  import cats.syntax.flatMap.*
  import cats.syntax.functor.*

  def generalRace[F[_], A, B](fa: F[A], fb: F[B])(using spawn: Spawn[F]): F[Either[A, B]] = {
    spawn.racePair(fa, fb).flatMap {
      case Left((outputA, fiberB)) => outputA match {
        case Succeeded(effectA) => fiberB.cancel >> effectA.map(a => Left(a))
        case Errored(e) => fiberB.cancel >> spawn.raiseError(e)
        case Canceled() => fiberB.join.flatMap {
          case Succeeded(effectB) => effectB.map(b => Right(b))
          case Errored(e) => spawn.raiseError(e)
          case Canceled() => spawn.raiseError(new RuntimeException("Both fibers were canceled"))
        }
      }
      case Right((fiberA, outputB)) => outputB match {
        case Succeeded(effectB) => fiberA.cancel >> effectB.map(b => Right(b))
        case Errored(e) => fiberA.cancel >> spawn.raiseError(e)
        case Canceled() => fiberA.join.flatMap {
          case Succeeded(effectA) => effectA.map(a => Left(a))
          case Errored(e) => spawn.raiseError(e)
          case Canceled() => spawn.raiseError(new RuntimeException("Both fibers were canceled"))
        }
      }
    }
  }

  import cats.effect.syntax.all.*
  import com.rockthejvm.utils.*

  import scala.concurrent.duration.*

  val fastEffect = IO.sleep(1.second) >> IO("Fast effect").debug
  val slowEffect = IO.sleep(3.seconds) >> IO("Slow effect").debug

  val raceResult: IO[Either[String, String]] = generalRace(fastEffect, slowEffect)



  override def run: IO[Unit] = raceResult.void

}
