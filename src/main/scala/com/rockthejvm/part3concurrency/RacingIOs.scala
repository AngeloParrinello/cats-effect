package com.rockthejvm.part3concurrency

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.kernel.{Fiber, Outcome}
import cats.effect.{IO, IOApp}

import scala.concurrent.duration.*
import scala.concurrent.duration.FiniteDuration

object RacingIOs extends IOApp.Simple {

  import com.rockthejvm.utils._

  def runWithSleep[A](value: A, duration: FiniteDuration): IO[A] = {
    (
    IO(s"Starting computation: $value").debug >>
    IO.sleep(duration) >>
    IO(s"Finished computation: $value").debug >>
    IO(value)
    ).onCancel(IO(s"Cancelling computation: $value").debug.void)
  }

  def testRace() = {
    val meaningOfLife = runWithSleep(42, 1.second)
    val favlang = runWithSleep("Scala", 2.seconds)
    val first: IO[Either[Int, String]] = IO.race(meaningOfLife, favlang)

    /*
    - both IOs run on separate fibers (threads)
    - the first one to finish wins (will complete the result)
    - the loser will be cancelled
     */
    first.flatMap {
      case Left(meaning) => IO(s"Meaning of life won: $meaning")
      case Right(lang) => IO(s"Favorite language won: $lang")
    }
  }

  def testRacePair() = {
    val meaningOfLife = runWithSleep(42, 1.second)
    val favlang = runWithSleep("Scala", 2.seconds)
    val first: IO[Either[
      (Outcome[IO, Throwable, Int], Fiber[IO, Throwable, String]), // (winner result, loser fiber)
      (Fiber[IO, Throwable, Int], Outcome[IO, Throwable, String]) // (loser fiber, winner result)
    ]]
    = IO.racePair(meaningOfLife, favlang)

    /*
    - both IOs run on separate fibers (threads)
    - the first one to finish wins (will complete the result)
    - but you do not lose the loser in this case, because it returns the result of the winner and the fiber of the loser!
    - so we can decide what to do with the loser

     */
    first.flatMap {
      case Left((meaning, fiberLanguage)) => IO(s"Meaning of life won: $meaning") >> fiberLanguage.cancel
      case Right((fiberMeaning, lang)) => IO(s"Favorite language won: $lang") >> fiberMeaning.cancel
    }
  }

  /**
   * Exercises:
   * 1 - implement a timeout pattern with race
   * @return
   */
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    val timeout = IO.sleep(duration)
    IO.race(io, timeout).flatMap {
      case Left(v) => IO(v)
      case Right(_) => IO.raiseError(new RuntimeException(s"IO timed out after $duration"))
    }
  }

  val importantTask = IO.sleep(2.seconds) >> IO(42).debug
  val testTimeout = timeout(importantTask, 1.second)
  // there is an API for timeout in IO, because it is a common pattern
  val testTimeoutV2 = importantTask.timeout(1.second)



  // 2 a method to return a losing effect from a race (hint: use racePair)
  def unrace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =
    IO.racePair(ioa, iob).flatMap {
      case Left((_, fiberB)) => fiberB.join.flatMap {
        case Succeeded(resultEffect) => resultEffect.map(result => Right(result))
        case Errored(e) => IO.raiseError(e)
        case Canceled() => IO.raiseError(new RuntimeException("Loser canceled"))

      }
      case Right((fiberA, _)) => fiberA.join.flatMap {
        case Succeeded(resultEffect) => resultEffect.map(result => Left(result))
        case Errored(e) => IO.raiseError(e)
        case Canceled() => IO.raiseError(new RuntimeException("Loser canceled"))
      }
    }

  // 3 - implement race in terms of racePair
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
    
    // whenever you call race, the piece of code above will be executed...a lot of code!
  }

  override def run: IO[Unit] = testRace().debug.void
}
