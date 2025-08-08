package com.rockthejvm.part4coordination

import cats.effect.kernel.Deferred
import cats.effect.std.CyclicBarrier
import cats.effect.{IO, IOApp, Ref}
import cats.syntax.all.*
import com.rockthejvm.utils.*

import scala.concurrent.duration.*
import scala.util.Random


object CyclicBarriers extends IOApp.Simple {

  // Cyclic barriers are a concurrency primitive that allows multiple fibers to wait for each other
  // until a certain point in time, at which all fibers are released to continue execution.

  // example: signing up for a social network just
  def createUser(id: Int, barrier: MyCyclicBarrier): IO[Unit] = for {
    _ <- IO.sleep(Random.nextInt(5).seconds)
    _ <- IO(s"User $id waiting for others to sign up...").debug
    _ <- IO.sleep(1.second)
    _ <- IO(s"User $id ready to sign up!").debug
    _ <- barrier.await // this will block the fiber until all fibers reach this point
    _ <- IO(s"User $id signed up!").debug
  } yield ()

  def openNetwork(): IO[Unit] = for {
    _ <- IO("Opening social network...").debug
    barrier <- MyCyclicBarrier(10) // create a cyclic barrier with 10 permits
    _ <- (1 to 20).toList.parTraverse(id => createUser(id, barrier)) // change the value from 7 to 14 and then 20 to see how the cyclic barrier works
    _ <- IO("Social network is now open!").debug
  } yield ()

  /**
   *
   * Exercise: Implement your own cyclic barrier with ref + deferred
   */

  trait MyCyclicBarrier {
    def await: IO[Unit] // blocks the fiber until all fibers reach this point
  }

  object MyCyclicBarrier {
    case class CycleState(countLeft: Int, signal: Deferred[IO, Unit])


    def apply(count: Int): IO[MyCyclicBarrier] =
      IO {
        new MyCyclicBarrier {
          override def await: IO[Unit] = for {
            signal <- Deferred[IO, Unit]
            state <- Ref[IO].of(CycleState(count, signal))
          } yield new MyCyclicBarrier {
            override def await: IO[Unit] =
              Deferred[IO, Unit].flatMap { newSignal =>
                for {
                  _ <- state.modify { currentState =>
                    if (currentState.countLeft == 1) {
                      (CycleState(count, newSignal), signal.complete(())) // last fiber to reach the barrier, complete the signal
                    } else {
                      (currentState.copy(countLeft = currentState.countLeft - 1), IO.unit) // decrement the count and return unit
                    }
                  }.flatten
                  _ <- signal.get
                } yield ()
              }
          }
        }
      }
  }


  override def run = openNetwork().debug.void

}
