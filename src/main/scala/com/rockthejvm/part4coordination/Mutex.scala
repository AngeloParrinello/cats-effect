package com.rockthejvm.part4coordination

import cats.effect
import cats.effect.kernel.{Deferred, Outcome}
import cats.effect.kernel.Outcome.{Canceled, Succeeded}
import cats.effect.kernel.Resource.ExitCase.Errored
import cats.effect.{IO, IOApp, Ref}
import cats.syntax.all.*
import com.rockthejvm.utils.*

import scala.collection.immutable.Queue
import scala.concurrent.duration.*
import scala.util.Random


abstract class Mutex {

  // acquire the mutex, blocking until it is available
  def acquire(): IO[Unit]

  // release the mutex
  def release(): IO[Unit]
}

object Mutex {
  type Signal = Deferred[IO, Unit]

  case class State(locked: Boolean, waiting: Queue[Signal])

  val unlocked: State = State(locked = false, waiting = Queue())

  def create: IO[Mutex] = {
    Ref[IO].of(unlocked).flatMap { state =>
      createMutexWithCancellation(state) // create a mutex with cancellation support
    }
  }

  def createMutexWithCancellation(state: Ref[IO, State]): IO[Mutex] = {
    Ref[IO].of(unlocked).map {
      state =>
        new Mutex {
          override def acquire(): IO[Unit] = IO.uncancelable { poll =>
            effect.Deferred[IO, Unit].flatMap { signal =>
              def cleanUp =
                state.modify {
                  case State(locked, queue) =>
                    val newQueue = queue.filterNot(_ == signal) // remove the signal from the queue if it is cancelled
                    (State(locked, newQueue), release())
                }.flatten // clean up the state if the signal is cancelled
                
              // create a Deferred to block on
              state.modify {
                case State(false, _) =>
                  // mutex is unlocked, lock it
                  (State(locked = true, Queue.empty), IO.unit)
                case State(true, waiting) =>
                  // mutex is locked, create a Deferred and add it to the waiting queue
                  (State(locked = true, waiting.enqueue(signal)), poll(signal.get).onCancel(cleanUp))
              }
            }.flatten
          }


          override def release(): IO[Unit] =
            state.modify {
              case State(false, _) =>
                // mutex is already unlocked, do nothing
                (unlocked, IO.unit)
              case State(true, waiting) if waiting.isEmpty =>
                // mutex is locked but no one is waiting, unlock it
                (unlocked, IO.unit)
              case State(true, waiting) =>
                // mutex is locked and someone is waiting, complete the first signal in the queue
                val (nextSignal, rest) = waiting.dequeue // dequeue the first signal
                (State(locked = true, rest), nextSignal.complete(()).void) // complete the signal and keep the mutex locked
            }.flatten
        }
    }
  }

  def createSimpleMutex(state: Ref[IO, State]): IO[Mutex] = {
    Ref[IO].of(unlocked).map {
      state =>
        new Mutex {

          /*
          Change the state of the Ref
          - if the mutex is currently unlocked, set it to locked and return immediately, state becomes State(locked = true, waiting = Queue.empty)
          - if the mutex is currently locked, create a Deferred[IO, Unit] and add it to the waiting queue
            - block until the Deferred is completed (which will happen when the mutex is released)
            - then re-check the state of the mutex
           */
          override def acquire(): IO[Unit] = {
            effect.Deferred[IO, Unit].flatMap { signal =>
              // create a Deferred to block on
              state.modify {
                case State(false, _) =>
                  // mutex is unlocked, lock it
                  (State(locked = true, Queue.empty), IO.unit)
                case State(true, waiting) =>
                  // mutex is locked, create a Deferred and add it to the waiting queue
                  (State(locked = true, waiting.enqueue(signal)), signal.get) // enqueue the signal and block on it
              }
            }.flatten
          }


          /*
          Change the state of the Ref:
          - IF THE MUTEX IS UNLOCKED, leave the state unchanged
          - if the mutex is locked,
          - if the queue is empty, unlock the mutex, set the state to unlocked (State(locked = false, waiting = Queue.empty))
          - if the queue is not empty, remove the first Deferred from the queue and complete it, setting the state to State(locked = true, waiting = Queue(rest of the signals))
           */
          override def release(): IO[Unit] =
            state.modify {
              case State(false, _) =>
                // mutex is already unlocked, do nothing
                (unlocked, IO.unit)
              case State(true, waiting) if waiting.isEmpty =>
                // mutex is locked but no one is waiting, unlock it
                (unlocked, IO.unit)
              case State(true, waiting) =>
                // mutex is locked and someone is waiting, complete the first signal in the queue
                val (nextSignal, rest) = waiting.dequeue // dequeue the first signal
                (State(locked = true, rest), nextSignal.complete(()).void) // complete the signal and keep the mutex locked
            }.flatten
        }
    }
  }
}

object MutexPlayground extends IOApp.Simple {
  def criticalTask(): IO[Int] = IO.sleep(1.second) *> IO(Random.nextInt(100)).debug

  def createNonLockingTask(id: Int): IO[Int] = for {
    _ <- IO(s"Task $id started").debug
    res <- criticalTask()
    _ <- IO(s"Task $id completed with result $res").debug
  } yield res

  def demoNonLockingTasks(): IO[List[Int]] = {
    (1 to 10).toList.parTraverse(id => createNonLockingTask(id))
  }

  def createLockingTask(id: Int, mutex: Mutex): IO[Int] = for {
    _ <- IO(s"Task $id trying to acquire mutex").debug
    _ <- mutex.acquire() // block until the mutex is acquired
    res <- criticalTask()
    _ <- IO(s"Task $id completed with result $res, releasing mutex").debug
    _ <- mutex.release() // release the mutex
  } yield res

  def demoLockingTasks(): IO[List[Int]] = {
    for {
      mutex <- Mutex.create // create a mutex
      results <- (1 to 10).toList.parTraverse { id =>
        createLockingTask(id, mutex)
      }
    } yield results
  }
  
  def createCancellingTask(id: Int, mutex: Mutex): IO[Int] =
    if (id % 2 == 0) {
      createLockingTask(id, mutex)
    } else {
      for {
        fib <- createLockingTask(id, mutex).onCancel(IO(s"Task $id cancelled").debug.void).start // start the task in a separate fiber
        _ <- IO.sleep(2.seconds) >> fib.cancel // cancel the task after 2 seconds
        _ <- IO(s"Task $id cancelled after 2 seconds").debug
        out <- fib.join // wait for the fiber to finish
        result <- out match {
          case Succeeded(effect) => effect // if the task completed successfully, return the result
          case Outcome.Errored(e) => IO(s"Task $id failed with error: $e").debug *> IO(-1) // if the task failed, return -1
          case Canceled() => IO(-2) // if the task was cancelled, return -2
        }
      } yield result
    }
  // only one task will proceed at a time, others will wait for the mutex to be released
  
  def demoCancellingTasks(): IO[List[Int]] = {
    for {
      mutex <- Mutex.create // create a mutex
      results <- (1 to 10).toList.parTraverse { id =>
        createCancellingTask(id, mutex)
      }
    } yield results
  }


  override def run = demoCancellingTasks().debug.void
}
