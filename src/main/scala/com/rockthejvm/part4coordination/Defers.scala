package com.rockthejvm.part4coordination

import cats.effect.unsafe.implicits.global
import cats.effect.{Deferred, IO, IOApp}
import cats.syntax.parallel.*
import com.rockthejvm.utils.*

import scala.concurrent.duration.*

object Defers extends IOApp.Simple {

  // deferred is a primitive that allows you to defer the execution of an effect until it is needed
  // waiting for an effect, while some other effect completes with a value

  val aDeferred: IO[Deferred[IO, Int]] = Deferred[IO, Int] // a deferred value that will be completed with an Int
  val aDeferredV2: IO[Deferred[IO, Int]] = IO.deferred[Int] // another way to create a deferred value

  // get blocks the calling fiber (semantically) until some other fiber completes the deferred value
  val reader: IO[Int] = aDeferred.flatMap { signal =>
    signal.get // this will block the fiber until the deferred value is completed
  }

  val writer: IO[Boolean] = aDeferred.flatMap { signal =>
    signal.complete(42) // this will complete the deferred value after 2 seconds
  }

  def demoDeferred(): IO[Unit] = {
    def consumer(signal: Deferred[IO, Int]): IO[Unit] = {
      for {
        _ <- IO("Consumer waiting for value...").debug
        meaningOfLife <- signal.get // this will block the fiber until the deferred value is completed
        _ <- IO(s"Consumer received value: $meaningOfLife").debug
      } yield ()
    }

    def producer(signal: Deferred[IO, Int]): IO[Unit] = {
      for {
        _ <- IO("Producer computing value...").debug
        _ <- IO.sleep(2.seconds) // simulate some computation
        _ <- signal.complete(42) // complete the deferred value
        _ <- IO("Producer completed value").debug
      } yield ()
    }

    for {
      signal <- Deferred[IO, Int] // create a deferred value
      _ <- (consumer(signal), producer(signal)).parTupled // run both consumer and producer in parallel
    } yield ()

  }

  override def run = {
    demoDeferred().debug.void
  }

}
