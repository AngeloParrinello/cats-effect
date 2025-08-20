package com.rockthejvm.part5polymorphic

import cats.effect
import cats.effect.*
import cats.effect.kernel.Deferred
import cats.syntax.flatMap.*

import scala.collection.immutable.Queue

object PolymorphicCoordination extends IOApp.Simple {

  // Concurrent - Ref + Deferred for any effect type F[_]
  trait MyConcurrent[F[_]] extends Spawn[F] {
    def ref[A](a: A): F[Ref[F, A]]

    def deferred[A]: F[Deferred[F, A]]
  }

  private val concurrentIO = Concurrent[IO]
  val aDeferred: IO[Deferred[IO, Int]] = Deferred[IO, Int] // requires an implicit Concurrent[IO] in scope
  val aDeferredV2: IO[Deferred[IO, Int]] = concurrentIO.deferred[Int] // using the Concurrent type class
  val aRef: IO[Ref[IO, Int]] = concurrentIO.ref(42) // requires an implicit Concurrent[IO] in scope

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

  private def polymorphicAlarmNotificationSystem[F[_]](using concurrent: Concurrent[F]): F[Unit] = {
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

  // Exercise
  // Generalize the following code
  // Generalize the Mutex concurrency primitive for any effect type F[_]
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

  private type GenRaceResult[F[_], A, B] = Either[
    (Outcome[F, Throwable, A], Fiber[F, Throwable, B]), // (winner result, loser fiber)
    (Fiber[F, Throwable, A], Outcome[F, Throwable, B]) // (loser fiber, winner result)
  ]

  private type GenEitherOutcome[F[_], A, B] = Either[Outcome[F, Throwable, A], Outcome[F, Throwable, B]]

  import cats.effect.syntax.monadCancel.*
  import cats.effect.syntax.spawn.*
  import cats.syntax.flatMap.*
  import cats.syntax.functor.*
  import com.rockthejvm.utils.general.*

  import scala.concurrent.duration.*

  // Actual implementation that you can find in GentConcurrent.scala in cats-effect
  def genOutRacePair[F[_], A, B](fa: F[A], fb: F[B])(using concurrent: cats.effect.Concurrent[F]): F[GenRaceResult[F, A, B]] = concurrent.uncancelable { poll =>
    for {
      signal <- concurrent.deferred[GenEitherOutcome[F, A, B]]
      fibA <- fa.guaranteeCase(outcomeA => signal.complete(Left(outcomeA)).void).start
      fibB <- fb.guaranteeCase(outcomeB => signal.complete(Right(outcomeB)).void).start
      result <- poll(signal.get).onCancel {
        for {
          cancelFibA <- fibA.cancel.start
          cancelFibB <- fibB.cancel.start
          _ <- cancelFibA.join
          _ <- cancelFibB.join
          _ <- concurrent.pure("Both fibers cancelled").debug
        } yield ()
      }
    } yield result match {
      case Left(outcomeA) => Left((outcomeA, fibB))
      case Right(outcomeB) => Right((fibA, outcomeB))
    }
  }

  // ex 2
  abstract class GenMutex[F[_]] {
    def acquire(): F[Unit]

    def release(): F[Unit]
  }

  object GenMutex {
    private type Signal[F[_]] = Deferred[F, Unit]

    private case class State[F[_]](locked: Boolean, waiting: Queue[Signal[F]])

    private def unlocked[F[_]]: State[F] = State[F](locked = false, waiting = Queue())

    private def createSignal[F[_]](using concurrent: Concurrent[F]): F[Signal[F]] =
      concurrent.deferred[Unit]

    def create[F[_]](using concurrent: Concurrent[F]): F[GenMutex[F]] = {
      concurrent.ref(unlocked[F]).map(initialState => createMutexWithCancellation(initialState))
    }

    private def createMutexWithCancellation[F[_]](state: Ref[F, State[F]])(using concurrent: Concurrent[F]): GenMutex[F] = {

      new GenMutex {
        override def acquire(): F[Unit] = concurrent.uncancelable { poll =>
          createSignal.flatMap { signal =>
            def cleanUp =
              state.modify {
                case State(locked, queue) =>
                  val newQueue = queue.filterNot(_ == signal) // remove the signal from the queue if it is cancelled
                  (State[F](locked, newQueue), release())
              }.flatten // clean up the state if the signal is cancelled

            // create a Deferred to block on
            state.modify {
              case State(false, _) =>
                // mutex is unlocked, lock it
                (State[F](locked = true, Queue.empty), concurrent.unit)
              case State(true, waiting) =>
                // mutex is locked, create a Deferred and add it to the waiting queue
                (State[F](locked = true, waiting.enqueue(signal)), poll(signal.get).onCancel(cleanUp))
            }
          }.flatten
        }


        override def release(): F[Unit] =
          state.modify {
            case State(false, _) =>
              // mutex is already unlocked, do nothing
              (unlocked, concurrent.unit)
            case State(true, waiting) if waiting.isEmpty =>
              // mutex is locked but no one is waiting, unlock it
              (unlocked, concurrent.unit)
            case State(true, waiting) =>
              // mutex is locked and someone is waiting, complete the first signal in the queue
              val (nextSignal, rest) = waiting.dequeue // dequeue the first signal
              (State[F](locked = true, rest), nextSignal.complete(()).void) // complete the signal and keep the mutex locked
          }.flatten
      }
    }
  }


  override def run: IO[Unit] =
    polymorphicAlarmNotificationSystem[IO].debug.void

}
