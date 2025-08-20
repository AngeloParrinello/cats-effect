package com.rockthejvm.part5polymorphic

import cats.Defer
import cats.effect.kernel.{Concurrent, Sync}
import cats.effect.{IO, IOApp, MonadCancel}
import cats.syntax.all.*
import com.rockthejvm.utils.general.*

import scala.concurrent.duration.*

object PolymorphicSync extends IOApp.Simple {

  val aDelayedComputation: IO[Int] = {
    IO.delay { // "Suspend" the computation in IO, until the effect is evaluated
      println("Computing...")
      42
    }
  }

  val aBlockingComputation: IO[Int] = {
    // Simulate a blocking computation, on some specific thread pool for blocking computations
    IO.blocking {
      println("Blocking computation...")
      Thread.sleep(1000)
      42
    }
  }

  // Synchronous computations, in cats-effect is Sync[F]
  // the combination of delay, blocking, and defer
  // allows you to run computation that are defined outside the context of cats-effect
  // inside the context of cats-effect!
  trait MySync[F[_]] extends MonadCancel[F, Throwable] with Defer[F] {
    def delay[A](thunk: => A): F[A] // "suspension" of a computation - will run on the CE thread pool

    def blocking[A](thunk: => A): F[A] // runs on the blocking thread pool

    // how would you implement this in terms of delay/blocking?
    def defer[A](fa: => F[A]): F[A] =
      // or flatMap(delay(fa))(identity)
      flatMap(fa)(a => delay(a))
  }

  val syncIO = Sync[IO] // given Sync[IO] instance in scope
  // abilities pure, map/flatMap, raiseError, handleError, uncancelable, etc. + delay/blocking
  val aDelayedIOV2 = syncIO.delay {
    println("Computing...")
    42
  } // same as IO.delay
  val aBlockingIOV2 = syncIO.blocking {
    println("Blocking computation...")
    Thread.sleep(1000)
    42
  } // same as IO.blocking

  val aDeferredIO = IO.defer(aDelayedIOV2)

  // Exercise: write a polymorphic console
  trait Console[F[_]] {
    def readLine(): F[String] // read a line from the console

    def printLine[A](line: A): F[A] // print a line to the console
  }

  // create a factory method that build a console instance for a particular type F for which there is an implicit/using
  // instance of Sync[F] in scope
  object Console {
    def apply[F[_]](using sync: Sync[F]): F[Console[F]] = sync.pure(System.out).map {
      out =>
        new Console[F] {
          override def readLine(): F[String] = sync.delay {
            println("Please enter a line:")
            scala.io.StdIn.readLine()
          }

          override def printLine[A](line: A): F[A] = sync.delay {
            out.println(line)
            line
          }
        }
    }
  }
//    def apply[F[_]](using sync: Sync[F]): F[Console[F]] = sync.pure[F]{
//      new Console[F] {
//        override def printLine[A](line: A): F[A] =
//          sync.delay(line)
//
//        override def readLine(): F[String] =
//          sync.blocking {
//            println("Please enter a line:")
//            scala.io.StdIn.readLine()
//          }
//    }
//  }


  def consoleReader() = for {
    console <- Console[IO]
    _ <- console.printLine("Welcome to the console reader!").debug
    line <- console.readLine()
    _ <- console.printLine(s"You entered: $line").debug
  } yield ()


  override def run: IO[Unit] =
    consoleReader()
}
