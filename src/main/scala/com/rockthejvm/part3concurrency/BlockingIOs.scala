package com.rockthejvm.part3concurrency

import cats.effect.{IO, IOApp}
import com.rockthejvm.utils.*

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*

object BlockingIOs extends IOApp.Simple {

  // the unit print will happen on different threads
  val someSleeps = for {
    _ <- IO.sleep(1.second).debug // SEMANTIC BLOCKING: means that the IO is not doing anything useful, but it is not blocking the thread
    _ <- IO.sleep(2.seconds).debug // means more that after 1 second, the thread will be free to do other things, but no actual thread is blocking
  } yield ()

  // really blocking IOs
  val aBlockingIO = IO.blocking {
    // this code will block the thread
    Thread.sleep(5000)
    println("Blocking IO completed " + Thread.currentThread().getName)
    42
  }

  // yielding
  val iosOnManyThreads = for {
    _ <- IO("first").debug
    _ <- IO.cede // a signal to yield control over the thread i.e. we signal to the IO runtime to start the rest of comuptation on another thread
    _ <- IO("second").debug
    _ <- IO.cede // we can yield multiple times
    _ <- IO("third").debug
  } yield ()

  // but at the first run, the IO runtime will use the same thread to run the IOs
  // optimization operations

  def testThousandEffectSwitch = {
    // and we define another execution context, the IO runtime will use different threads
    val ec: ExecutionContext = ExecutionContext.fromExecutorService(
      java.util.concurrent.Executors.newFixedThreadPool(8)
    )
    // but if we run several times the cede method...
    (1 to 1000).map(IO.pure).reduce(_.debug >> IO.cede >> _.debug).evalOn(ec)
  }

  override def run: IO[Unit] = {
    // someSleeps >> aBlockingIO >> iosOnManyThreads
    // aBlockingIO.debug >> iosOnManyThreads
    //iosOnManyThreads
    testThousandEffectSwitch.void
  }

}
