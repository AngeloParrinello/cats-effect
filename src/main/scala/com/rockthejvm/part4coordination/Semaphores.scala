package com.rockthejvm.part4coordination

import cats.effect.std.Semaphore
import cats.effect.{IO, IOApp}
import com.rockthejvm.utils.*

import scala.concurrent.duration.*
import scala.util.Random
import cats.syntax.parallel.*

object Semaphores extends IOApp.Simple {


  // Semaphores are a concurrency primitive that allow you to control access to a resource

  val semaphore: IO[Semaphore[IO]] = Semaphore[IO](2) // 2 total permits

  // ex: limiting the number of concurrent requests to a service
  def doWorkWhileLoggedIn(): IO[Int] = IO.sleep(1.second) *> IO(Random.nextInt(100))

  def login(id: Int, sem: Semaphore[IO]): IO[Int] = for {
    _ <- IO(s"User $id trying to log in").debug
    _ <- sem.acquire
    // critical section
    _ <- IO(s"User $id logged in").debug
    res <- doWorkWhileLoggedIn()
    _ <- IO(s"User $id logging out" + s" with result $res").debug
    // release the permit and end of critical section
    _ <- sem.release // release the permit
  } yield res

  def demoSemaphore() = {
    for {
      sem <- Semaphore[IO](2) // create a semaphore with 2 permits
      user1Fib <- login(1, sem).start // start user 1 in a separate fiber
      user2Fib <- login(2, sem).start // start user 2 in a
      user3Fib <- login(3, sem).start // start user 3 in a separate fiber
      _ <- user1Fib.join // wait for user 1 to finish
      _ <- user2Fib.join // wait for user 2 to finish
      _ <- user3Fib.join // wait for user 3 to finish
    } yield ()
  }

  def weightedLogin(id: Int, requiredPermits: Int, sem: Semaphore[IO]): IO[Int] = for {
    _ <- IO(s"User $id trying to log in with $requiredPermits permits").debug
    // The fiber will be blocked until the required number of permits are available
    // Is an all or nothing operation
    _ <- sem.acquireN(requiredPermits) // acquire multiple permits
    // critical section
    _ <- IO(s"User $id logged in with $requiredPermits permits").debug
    res <- doWorkWhileLoggedIn()
    _ <- IO(s"User $id logging out with result $res").debug
    // release the permits and end of critical section
    _ <- sem.releaseN(requiredPermits) // release multiple permits
  } yield res

  def demoWeightedSemaphore() = {
    for {
      sem <- Semaphore[IO](2) // create a semaphore with 2 permits
      user1Fib <- weightedLogin(1,1, sem).start // start user 1 in a separate fiber
      user2Fib <- weightedLogin(2,2, sem).start // start user 2 in a
      user3Fib <- weightedLogin(3, 3,sem).start // start user 3 in a separate fiber
      _ <- user1Fib.join // wait for user 1 to finish
      _ <- user2Fib.join // wait for user 2 to finish
      _ <- user3Fib.join // wait for user 3 to finish
    } yield ()
  }


  // exercise
  // semaphore with 1 permit == mutex
  // 1 find out if there is something wrong with this code
  // 2 why?
  // 3 fix it
  val mutex: IO[Semaphore[IO]] = Semaphore[IO](1)
  val users: IO[List[Int]] = (1 to 10).toList.parTraverse {
    id =>
      for {
        sem <- mutex // get the semaphore
        _ <- IO(s"User $id trying to log in").debug
        _ <- sem.acquire
        // critical section
        _ <- IO(s"User $id logged in").debug
        res <- doWorkWhileLoggedIn()
        _ <- IO(s"User $id logging out" + s" with result $res").debug
        // release the permit and end of critical section
        _ <- sem.release // release the permit
      } yield res
  }

  // 1. Expected: all tasks start at the same time, but only 1 task can run at a time
  //    Actual: all tasks are parallel

  // 2
  // Why? We flatMap Semaphore[IO](1) inside the parTraverse, which means that each task gets its own semaphore

  // 3
  // Fix: move the semaphore outside the parTraverse, so that all tasks share the same semaphore
  val usersFixed: IO[List[Int]] = mutex.flatMap { sem =>
    (1 to 10).toList.parTraverse {
      id =>
        for {
          _ <- IO(s"User $id trying to log in").debug
          _ <- sem.acquire
          // critical section
          _ <- IO(s"User $id logged in").debug
          res <- doWorkWhileLoggedIn()
          _ <- IO(s"User $id logging out" + s" with result $res").debug
          // release the permit and end of critical section
          _ <- sem.release // release the permit
        } yield res
    }
  }



  override def run = usersFixed.debug.void
}
