package com.rockthejvm.part5polymorphic

import cats.effect.*
import cats.effect.kernel.{Concurrent, Deferred}

object PolymorphicCoordination extends IOApp.Simple {

  // Concurrent - Ref + Deferred for any effect type F[_]
  trait MyConcurrent[F[_]] extends Spawn[F] {
    def ref[A](a: A): F[Ref[F, A]]

    def deferred[A]: F[Deferred[F, A]]
  }

  val concurrentIO = Concurrent[IO]
  val aDeferred = Deferred[IO, Int] // requires an implicit Concurrent[IO] in scope
  val aDeferredV2 = concurrentIO.deferred[Int] // using the Concurrent type class
  val aRef = concurrentIO.ref(42) // requires an implicit Concurrent[IO] in scope

  // so in this way we have a more powerful type class: pure, map/flatMap, raiseError, uncancelable, canceled, start (fibers), cede, racePair, ref, deferred

  // previous exercise on Defers
  // to generalize

  import cats.effect.syntax.spawn.*
  import cats.syntax.flatMap.*
  import cats.syntax.functor.*
  import com.rockthejvm.utils.general.*

  import scala.concurrent.duration.*

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

  def polymorphicAlarmNotificationSystem[F[_]](using concurrent: Concurrent[F]): F[Unit] = {
    def alarmNotifier(signal: Deferred[F, Unit]): F[Unit] = {
      for {
        _ <- concurrent.pure("Waiting for alarm to be triggered...").debug
        _ <- signal.get // this will block the fiber until the deferred value is completed
        _ <- concurrent.pure("Alarm triggered! Notifying...").debug
      } yield ()
    }

    def counter(count: Ref[F, Int], signal: Deferred[F, Unit]): F[Unit] = {
      for {
        _ <- unsafeSleep(1.second) // simulate some computation
        currentCount <- count.updateAndGet(_ + 1)
        _ <- if (currentCount >= 10) {
          signal.complete(()).void // complete the deferred value when the count reaches 10
        } else {
          counter(count, signal) // continue counting
        }
      } yield ()
    }

    for {
      count <- concurrent.ref(0) // create a Ref to hold the count
      signal <- concurrent.deferred[Unit] // create a deferred value to signal when the alarm is triggered
      fibNotifier <- alarmNotifier(signal).start // start the notifier in a separate fiber
      fibCounter <- counter(count, signal).start // start the counter in a separate fiber
      _ <- fibCounter.join // wait for the counter to finish
      _ <- fibNotifier.join // wait for the notifier to finish
    } yield ()
  }


  override def run: IO[Unit] =
    polymorphicAlarmNotificationSystem[IO].debug.void

}
