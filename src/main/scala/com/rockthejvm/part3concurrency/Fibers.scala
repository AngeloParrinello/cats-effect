package com.rockthejvm.part3concurrency

import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}

import scala.concurrent.duration.*
import cats.effect.{Fiber, FiberIO, IO, IOApp}

object Fibers extends IOApp.Simple {

  val meaningOfLife = IO.pure(42)
  val favLang = IO.pure("Scala")

  import com.rockthejvm.utils._

  // This is a simple IO composition THAT IS EVALUATED ON THE SAME THREAD, SEQUENTIALLY VIA FLATMAP
  def simpleIOComposition: IO[Unit] = for {
    number <- meaningOfLife.debug
    lang <- favLang.debug
  } yield ()

  // introduce a fiber --> Fiber: is similar to the thread concept of JVM, except that a Fiber (for cats) represents a
  // computation that will run or schedule on a thread, managed by the cats-effect runtime
  def createFiber: Fiber[IO, Throwable, String] = ??? // almost impossible to create fibers manually
  // can be only created via some specific API
  // the fiber it's wrapped into a IO because the allocation fo a fiber it's effectful some might produce side effects
  // and we need to handle them
  // the fiber is not actually started, but the fiber allocation is wrapped in another effect
  val aFiber: IO[Fiber[IO, Throwable, Int]] = meaningOfLife.debug.start

  // the fiber is started, and the result is a fiber that will run in the background
  def differentThreadIOs = for {
    _ <- aFiber
    _ <- favLang.debug
  } yield ()

  // joining a fiber: waiting a fiber to finish
  def runOnSomeOtherThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
    fiber <- io.start // start the computation in a different thread
    result <- fiber.join // wait for the computation to finish and get the result, an effect which waits for the fiber to finish
  } yield result
  /*
  IO[ResultType of fib.join]
  fib.join = Outcome[IO, Throwable, A]
   */
  /*
  possible outcomes:
  - success weith an IO
  - failure with an exception
  - cancelled
   */

  val someIOOnAnotherThread = runOnSomeOtherThread(meaningOfLife)
  val someResultFromAnotherThread = someIOOnAnotherThread.flatMap {
    case Outcome.Succeeded(effect) => effect.debug
    case Outcome.Canceled() => IO(println("The fiber was cancelled"))
    case Outcome.Errored(exception) => IO(println(s"The fiber failed with exception: $exception"))
  }

  def throwOnAnotherThread = for {
    fib <- IO.raiseError(new RuntimeException("no number for you")).start
    result <- fib.join
  } yield result


  // when we run testCancel, we'll see that finished is never printed because the signal to cancel the fiber
  // arrives before the task is finished
  def testCancel = {
    val task = IO("starting").debug >> IO.sleep(1.second) >> IO("finished").debug

    for {
      fib <- task.start // start the task in a different thread
      _ <- IO.sleep(0.5.second) >> IO("cancelling the task").debug // wait for a bit and cancel the task, running on the main thread (calling thread)
      _ <- fib.cancel // cancel the task
      result <- fib.join // wait for the task to finish
    } yield result
  }

  def testCancelWithHandler = {
    val task = IO("starting").debug >> IO.sleep(1.second) >> IO("finished").debug
    val taskWithCancellationHandler = task.onCancel(IO("I'm being cancelled").debug.void) // and it also free up resources
    for {
      fib <- taskWithCancellationHandler.start // start the task in a different thread
      _ <- IO.sleep(0.5.second) >> IO("cancelling the task").debug // wait for a bit and cancel the task, running on the main thread (calling thread)
      _ <- fib.cancel // cancel the task
      result <- fib.join // wait for the task to finish
    } yield result
  }
  
  // Exercises
  import com.rockthejvm.utils._
  
  // ex. 1
  def processResultsFromFiber[A](io: IO[A]): IO[A] = {
    val res = for {
      fiber <- io.start // start the computation in a different thread
      result <- fiber.join // wait for the computation to finish and get the result, an effect which waits for the fiber to finish
    } yield result
    
    res.flatMap {
      case Outcome.Succeeded(effect) => effect // return the result of the computation
      case Outcome.Canceled() | Outcome.Errored(_) => IO.raiseError(new RuntimeException("The fiber was cancelled or errored")) // return an error
    }
  }


  // ex 2
  def tupleIOs[A, B](ioa: IO[A], iob: IO[B]): IO[(A, B)] =
    val res = for {
      fiberA <- ioa.start // start the first computation in a different thread
      fiberB <- iob.start // start the second computation in a different thread
      resultA <- fiberA.join // wait for the first computation to finish and get the result
      resultB <- fiberB.join // wait for the second computation to finish and get the result
    } yield (resultA, resultB) 
    res.flatMap {
      case (Outcome.Succeeded(effectA), Outcome.Succeeded(effectB)) => for {
        a <- effectA // return the result of the first computation
        b <- effectB // return the result of the second computation
      } yield (a, b) // return a tuple with both results
      case (Outcome.Canceled(), _) | (_, Outcome.Canceled()) => IO.raiseError(new RuntimeException("One of the fibers was cancelled")) // return an error
      case (Outcome.Errored(_), _) | (_, Outcome.Errored(_)) => IO.raiseError(new RuntimeException("One of the fibers errored")) // return an error
    }
  
  // ex 3
  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = {
//    val res = for {
//      fiberA <- io.start
//      fiberB <- IO.sleep(duration).start.flatMap {
//        case Outcome.Succeeded(_) => fiberA.cancel
//        case Outcome.Canceled() => IO.unit // return a success
//        case Outcome.Errored(_) => IO.unit // return a success
//      }
//      resultA <- fiberA.join // wait for the first computation to finish and get the result
//    } yield ???
    val computation = for {
      fib <- io.start
      _ <- IO.sleep(duration) >> fib.cancel
      result <- fib.join
    } yield result
    
    computation.flatMap {
      case Outcome.Succeeded(effect) => effect // return the result of the computation
      case Outcome.Errored(e) => IO.raiseError(e)
      case Outcome.Canceled() => IO.raiseError(new RuntimeException("The fiber was cancelled")) // return an error
    }
    
//    val res = for {
//      fiber <- io.start.onCancel(IO.raiseError(new RuntimeException("timeout invoked"))).timeout(duration) // start the computation in a different thread
//      timeout <- IO.sleep(duration).start.flatMap {
//        case Outcome.Succeeded(_) => IO.raiseError(new RuntimeException("timeout invoked")) // return an error
//        case Outcome.Canceled() => IO.unit // return a success
//        case Outcome.Errored(_) => IO.unit // return a success
//      }
//      result <- fiber.join
//    } yield result // wait for the computation to finish and get the result, an effect which waits for the fiber to finish
//    ???
  }




  override def run: IO[Unit] =
    // simpleIOComposition
    // differentThreadIOs
//    runOnSomeOtherThread(meaningOfLife) // IO(Succeeded(IO(42)))
//      .debug.void
//    throwOnAnotherThread.debug.void
//    testCancel.debug.void
    testCancelWithHandler.debug.void
}
