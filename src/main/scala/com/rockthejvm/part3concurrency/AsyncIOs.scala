package com.rockthejvm.part3concurrency

import cats.effect.{IO, IOApp}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import scala.concurrent.duration.*

object AsyncIOs extends IOApp.Simple {

  // Async IOs are not blocking, but they can be executed on different threads
  // and they can be executed in parallel

  // Async IOs are useful for non-blocking operations that can be executed in parallel
  // e.g. network calls, database calls, etc.

  // They run on fibers, which are lightweight threads managed by the IO runtime
  // but we don't need to worry about the fiber lifecycle, the IO runtime will take care of it

  import com.rockthejvm.utils.*

  val threadPool = Executors.newFixedThreadPool(8)
  val ec: scala.concurrent.ExecutionContext =
    scala.concurrent.ExecutionContext.fromExecutorService(threadPool)

  type Callback[A] = Either[Throwable, A] => Unit

  def computeMeaningOfLife(): Int = { // Simulating a blocking operation
    Thread.sleep(1000) // This is a blocking operation
    println(Thread.currentThread().getName + " - Computation done")
    42
  }

  def computeMeaningOfLifeEither: Either[Throwable, Int] = Try {
    computeMeaningOfLife()
  }.toEither

  def computeMolOnThreadPool(): Unit = {
    threadPool.execute(() => computeMeaningOfLifeEither)
  }

  // we want to lift this computation into an IO
  // async is a FFI (Foreign Function Interface) to lift blocking operations into IOs
  // but this computation is running on a fiber, which is a lightweight thread managed by the IO runtime
  val asyncIo: IO[Int] = IO.async_ {
    cb => // CE thread blocks semantically until this cb is invoked (by some other thread)
      // we can use the thread pool to run the blocking operation
      threadPool.execute(() => {
        val result = computeMeaningOfLifeEither
        cb(result) // notify that the computation has been computed, callback with the result
      })
    }

  /**
   * Exercise
   */
  def asyncToIO[A](computation: () => A)(ec: ExecutionContext): IO[A] =
    IO.async_[A] { (cb: Callback[A]) =>
      ec.execute {
        () =>
          val result = Try(computation()).toEither
          cb(result) // notify that the computation has been computed, callback with the result
      }
    }


  val asyncMolv2: IO[Int] = asyncToIO(computeMeaningOfLife)(ec)

  // Exercise: lift a Future (an async comp) into an IO
  lazy val molFuture: Future[Int] = Future { computeMeaningOfLife() }(ec)

  def convertFutureToIO[A](future: Future[A]): IO[A] = {
    IO.async_[A] { cb =>
      future.onComplete {
        case Success(value) => cb(Right(value))
        case Failure(exception) => cb(Left(exception))
      }(ec)
    }
  }

  // so popular that it is already implemented in the IO companion object
  val molFutureToIO: IO[Int] = IO.fromFuture(IO(molFuture))

  // Exercise: a never ending async IO
  // no callback, no finish
  def neverEndingAsyncIo: IO[Int] = IO.async_[Int] {_ => ()}

  // there is a never ending async IO in the IO companion object
  val neverEndingAsyncIoV2: IO[Int] = IO.never

  /*
  FULL ASYNC CALL
   */
  def demoAsyncCancellation() = {
    val asyncMeaningOfLifeV2: IO[Int] = IO.async {
      cb =>
        // we need to return IO[Option[IO[Unit]]]
        // because we want a finalizer in case computation gets cancelled
        // finalizers are of type IO[Unit]
        // and not specifying a finalizer is => Option[IO[Unit]]
        // but creating an option is an effect so ... => IO[Option[IO[Unit]]]
        // I need to return IO[Option[IO[Unit]]]
        IO {
          threadPool.execute {
            () =>
              // this is a blocking operation
              val result = computeMeaningOfLifeEither
              cb(result) // notify that the computation has been computed, callback with the result
          }
        }.as(Some(IO("Computation cancelled").debug.void)) // this is the finalizer, it will be executed if the IO is cancelled
    }
    
    for {
      fib <- asyncMeaningOfLifeV2.start // start the async IO
      _ <- IO.sleep(500.millis) >> IO("Attempting cancellation...").debug >> fib.cancel // cancel the async IO after 1 second
      _ <- fib.join // wait for the async IO to finish
    } yield ()
  }
  


  override def run: IO[Unit] = {
    // Example of an async IO operation
    // IO(println("Async IO operation")).debug
    // asyncIo.debug >> IO(threadPool.shutdown())
    demoAsyncCancellation().debug >> IO(threadPool.shutdown())
  }

}
