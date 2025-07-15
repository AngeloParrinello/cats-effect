package com.rockthejvm.part4coordination

import cats.effect.*
import cats.effect.kernel.Outcome
import cats.effect.unsafe.implicits.global
import cats.implicits.*
import cats.syntax.all.*
import cats.syntax.parallel.*
import com.rockthejvm.utils.*

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.Implicits.global
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


  // simulate downloading some content
  val fileParts = List("Start dowloading", "...", "part1", "part2", "part3", "part4", "part5<EOF>")

  def fileNotifierWithRef(): IO[Unit] = {
    def downloadFile(contentRef: Ref[IO, String]): IO[Unit] = {
      fileParts.map {
          part => IO(s"Got '$part").debug >> IO.sleep(1.second) >> contentRef.update(current => current + part)
        }
        .sequence
        .void
    }

    def notifyFileComplete(contentRef: Ref[IO, String]): IO[Unit] = {
      for {
        file <- contentRef.get
        _ <- if (file.endsWith("<EOF>")) {
          IO(s"File download complete: $file").debug
        } else {
          IO("File download incomplete, waiting...").debug >> IO.sleep(1.second) >> notifyFileComplete(contentRef)
        }
      } yield ()
    }

    for {
      contentRef <- Ref[IO].of("") // create a Ref to hold the file content
      fibDownloader <- downloadFile(contentRef).start // start the download in a separate fiber
      fibNotifier <- notifyFileComplete(contentRef).start // notify when the file is complete
      _ <- fibDownloader.join // wait for the downloader to finish
      _ <- fibNotifier.join // wait for the notifier to finish
    } yield ()
  }

  // ok but this above is fucking BUSY WAITING!
  // it is trade safe but it is not efficient

  // but with deferred we can do better...
  def fileNotifierWithDeferred(): IO[Unit] = {
    def notifyFileComplete(signal: Deferred[IO, String]): IO[Unit] = {
      for {
        _ <- IO("Waiting for file to be downloaded...").debug
        file <- signal.get // this will block the fiber until the deferred value is completed
        _ <- IO(s"File download complete: $file").debug
      } yield ()
    }

    def downloadFile(part: String, contentRef: Ref[IO, String], signal: Deferred[IO, String]): IO[Unit] =
      for {
        _ <- IO(s"Got '$part'").debug
        _ <- IO.sleep(1.second) // simulate some computation
        latestContent <- contentRef.updateAndGet(current => current + part) // update the content
        _ <- if (latestContent.endsWith("<EOF>")) {
          signal.complete(latestContent) // complete the deferred value when the file is complete
        } else {
          IO.unit // do nothing if the file is not complete
        }
      } yield ()

    for {
      contentRef <- Ref[IO].of("") // create a Ref to hold the file content
      signal <- Deferred[IO, String] // create a deferred value to signal when the file is complete
      fibNotifier <- notifyFileComplete(signal).start // start the notifier in a separate fiber
      fibFileTasks <- fileParts.map(part => downloadFile(part, contentRef, signal)).sequence.start // start all the download tasks in parallel
      _ <- fibFileTasks.join // wait for all the download tasks to finish
      _ <- fibNotifier.join // wait for the notifier to finish
    } yield ()
  }

  // Exercises:

  def alarmNotificationSystem(): IO[Unit] = {
    def alarmNotifier(signal: Deferred[IO, Unit]): IO[Unit] = {
      for {
        _ <- IO("Waiting for alarm to be triggered...").debug
        _ <- signal.get // this will block the fiber until the deferred value is completed
        _ <- IO("Alarm triggered! Notifying...").debug
      } yield ()
    }

    def counter(count: Ref[IO, Int], signal: Deferred[IO, Unit]): IO[Unit] = {
      for {
        _ <- IO.sleep(1.second) // simulate some computation
        currentCount <- count.updateAndGet(_ + 1) // increment the count
        _ <- if (currentCount >= 10) {
          signal.complete(()) // complete the deferred value when the count reaches 10
        } else {
          IO(s"Current count: $currentCount").debug >> counter(count, signal) // continue counting
        }
      } yield ()
    }

    for {
      count <- Ref[IO].of(0) // create a Ref to hold the count
      signal <- Deferred[IO, Unit] // create a deferred value to signal when the alarm is triggered
      fibNotifier <- alarmNotifier(signal).start // start the notifier in a separate fiber
      fibCounter <- counter(count, signal).start // start the counter in a separate fiber
      _ <- fibCounter.join // wait for the counter to finish
      _ <- fibNotifier.join // wait for the notifier to finish
    } yield ()
  }


  type RaceResult[A, B] = Either[
    (Outcome[IO, Throwable, A], Fiber[IO, Throwable, B]), // (winner result, loser fiber)
    (Fiber[IO, Throwable, A], Outcome[IO, Throwable, B]) // (loser fiber, winner result)
  ]
  type EitherOutcome[A, B] = Either[Outcome[IO, Throwable, A], Outcome[IO, Throwable, B]]

  def ourRacePair[A, B](ioa: IO[A], iob: IO[B]): IO[RaceResult[A, B]] = IO.uncancelable { poll =>
    for {
      signal <- Deferred[IO, EitherOutcome[A, B]] // create a deferred value to signal the result
      fibA <- ioa.guaranteeCase(outcomeA => signal.complete(Left(outcomeA)).void).start // start the first IO and complete the deferred value with its outcome
      fibB <- iob.guaranteeCase(outcomeB => signal.complete(Right(outcomeB)).void).start // start the second IO and complete the deferred value with its outcome
      result <- poll(signal.get).onCancel { // block until one of the fibers completes --> and this part should be cancelable
        for {
          cancelFibA <- fibA.cancel.start // cancel the first fiber
          cancelFibB <- fibB.cancel.start // cancel the second fiber
          _ <- cancelFibA.join // wait for the first fiber to finish
          _ <- cancelFibB.join // wait for the second fiber to finish
          _ <- IO("Both fibers cancelled").debug // debug message
        } yield ()
      }
    } yield result match {
      case Left(outcomeA) => Left((outcomeA, fibB)) // A won, B lost
      case Right(outcomeB) => Right((fibA, outcomeB)) // B won, A lost
    }
  }

  override def run = {
    // demoDeferred().debug.void
    // fileNotifierWithRef().debug.void
    //fileNotifierWithDeferred()
    alarmNotificationSystem()
  }

}
