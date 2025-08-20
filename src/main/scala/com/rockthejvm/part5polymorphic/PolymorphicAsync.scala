package com.rockthejvm.part5polymorphic

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.kernel.syntax.spawn.*
import cats.effect.kernel.{Concurrent, Outcome, Sync, Temporal}
import cats.effect.{Async, IO, IOApp}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.all.*
import com.rockthejvm.utils.general.*

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

object PolymorphicAsync extends IOApp.Simple {

  // Async - the most powerful type class in cats-effect
  // async computations, "suspended" in F
  trait MyAsync[F[_]] extends Sync[F] with Temporal[F] {
    // fundamental description of the Async type class
    def executionContext: F[ExecutionContext]
    def async[A](cb: (Either[Throwable, A] => Unit) => F[Option[F[Unit]]]): F[A]
    def evalOn[A](fa: F[A], ec: ExecutionContext): F[A]
    
    def async_[A](cb: (Either[Throwable, A] => Unit) => Unit): F[A] =
      async(cb2 => map(pure(cb(cb2)))(_ => None))
    def never[A]: F[A] = async_(_ => ()) // because the callback is never invoked, the effect never completes
  }

  val asyncIO = Async[IO] // fetch the Async type class for IO
  // abilities: pure, map/flatMap, raiseError, uncancelable, canceled, start, delay/defer/blocking +
  val ec = asyncIO.executionContext

  // power: async_ + async: FFI (Foreign Function Interface) to run computations in F[_] that are not pure
  // from an external context (in this case a thread pool) into the context of cats-effect
  val threadPool = Executors.newFixedThreadPool(10)
  type Callback[A] = Either[Throwable, A] => Unit
  val asyncMeaningOfLife = IO.async_ {
    (cb: Callback[Int]) =>
      // start the computation on another thread
      threadPool.execute {
        () =>
          println(s"Computing the meaning of life on thread: ${Thread.currentThread().getName}")
          cb(Right(42))
      }
  }

  val asyncMeaningOfLifeComplex = IO.async {
    (cb: Callback[Int]) =>
      // start the computation on another thread
      IO {
        threadPool.execute {
          () =>
              println(s"Computing the meaning of life on thread: ${Thread.currentThread().getName}")
              cb(Right(42))
        }
      }.as(Some(IO(println("Computation started!")).debug.void))
  }


  val myExecutionContext = ExecutionContext.fromExecutorService(threadPool)
  val asyncMeaningOfLifeV2: IO[Int] = asyncIO.evalOn(IO(42).debug, myExecutionContext).guarantee(IO(threadPool.shutdown()))


  // never
  val neverIO = asyncIO.never[Int] // a fiber that never completes, a forever suspended effect

  // Exercises
  // 1 - Implement never and async_ in terms of async
  // 2 - Tuple two effects with different requirements
  def firstEffect[F[_]: Concurrent, A](a: A): F[A] =
    Concurrent[F].pure(a)

  def secondEffect[F[_]: Sync, A](a: A): F[A] =
    Sync[F].pure(a)


  def tupledEffect[F[_]: Async, A](a: A): F[(A, A)] =
    for {
      first <- firstEffect[F, A](a)
      second <- secondEffect[F, A](a)
    } yield (first, second)
    


  override def run: IO[Unit] = ???
}
