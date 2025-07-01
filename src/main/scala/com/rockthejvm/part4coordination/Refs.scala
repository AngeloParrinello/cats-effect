package com.rockthejvm.part4coordination

import cats.effect.{ExitCode, IOApp, Ref}

object Refs extends IOApp.Simple {

  import cats.effect.IO
  import cats.effect.unsafe.implicits.global
  import cats.syntax.parallel.*
  import com.rockthejvm.utils.*

  import scala.concurrent.duration.*

  // Refs are mutable references that can be used in a concurrent environment
  // ref = purely functional atomic reference
  val atomicMol: IO[Ref[IO, Int]] = Ref[IO].of(42)
  val atomicMolV2: IO[Ref[IO, Int]] = IO.ref(42)

  // modify is an effect
  val increasedMol: IO[IO[Unit]] = atomicMol.map {
    ref =>
      ref.set(43) // set is an effect
  }
  val increasedMolV2: IO[Unit] = atomicMol.flatMap {
    ref =>
      ref.set(44) // always thread-safe
  }

  // obtain a value
  val mol: IO[Int] = atomicMol.flatMap {
    ref =>
      ref.get // get is an effect, but it is not blocking
  }
  val getAndSetMol: IO[Int] = atomicMol.flatMap {
    ref =>
      ref.getAndSet(45) // getAndSet is an effect that returns the previous value
  }

  // updating with a function
  val updatedMol: IO[Unit] = atomicMol.flatMap {
    ref =>
      ref.update(_ + 1) // update is an effect that returns Unit
  }
  val updatedMolV2: IO[Int] = atomicMol.flatMap {
    ref =>
      ref.updateAndGet(_ + 1) // updateAndGet is an effect that returns the new value, there is also a getAndUpdate
  }

  // modifying with a function returning a different type
  val modifiedMol: IO[String] = atomicMol.flatMap {
    ref =>
      ref.modify(mol => (mol + 1, s"New mol value: ${mol + 1}")) // modify is an effect that returns a tuple
  }

  // why do we need Refs?
  // concurrent + thread safe reads/writes over shared values, in a purely functional way

  def demoConcurrentWorkImpure(): IO[Unit] = {
    var count = 0 // this is an impure variable, not thread-safe

    def task(workload: String): IO[Unit] = {
      val wordCount = workload.split(" ").length
      for {
        _ <- IO(s"Counting words for $workload : $wordCount").debug
        newCount = count + wordCount // this is not thread-safe, if two threads run this at the same time, they might read the same count
        _ <- IO(s"New count: $newCount").debug
        _ = count = newCount // this is not thread-safe, if two threads run this at the same time, they might write the same count
      } yield ()
    }

    def task2(workload: String): IO[Unit] = {
      val wordCount = workload.split(" ").length
      for {
        _ <- IO(s"Counting words for $workload : $wordCount").debug
        newCount <- IO(count + wordCount) // this is not thread-safe, if two threads run this at the same time, they might read the same count
        _ <- IO(s"New count: $newCount").debug
        _ = IO(count += newCount) // this is not thread-safe, if two threads run this at the same time, they might write the same count
      } yield ()
    }

    // this code above is not thread-safe, if we run it in parallel, we might get inconsistent results
    // cons:
    // = hard tor ead debug
    // we use pure and impure code
    // not thread-safe!!

    List("I love cats", "Cats are great", "Functional programming is awesome")
      .map(task2)
      .parSequence // sequence is a method that runs all the IOs in sequence, but this is not thread-safe
      .void
  }


  def demoConcurrentWorkPure(): IO[Unit] = {
    // we can use Refs to make the code thread-safe
    val atomicCount: IO[Ref[IO, Int]] = Ref[IO].of(0)

    def task(workload: String, ref: Ref[IO, Int]): IO[Unit] = {
      val wordCount = workload.split(" ").length

      for {
        _ <- IO(s"Counting words for $workload : $wordCount").debug
        newCount <- ref.updateAndGet(currentCount => currentCount + wordCount) // this is thread-safe, it will atomically update the count
        _ <- IO(s"New count: $newCount").debug
      } yield ()
    }

    for {
      initCount <- IO.ref(0)
      _ <- List("I love cats", "Cats are great", "Functional programming is awesome")
        .map(string => task(string, initCount)) // we pass the Ref to the task
        .parSequence // sequence is a method that runs all the IOs in parallel, but this is thread-safe
    } yield ()
  }


  // Exercise
  def tickingClockImpure(): Unit = {
    var ticks = 0L

    def tickingClock: IO[Unit] = for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug
      _ = ticks += 1 // this is not thread-safe, if two threads run this at the same time, they might write the same count
      _ <- tickingClock // recursive call
    } yield ()

    def printTicks: IO[Unit] = for {
      _ <- IO.sleep(5.seconds)
      _ <- IO(s"Ticks: $ticks").debug // this is not thread-safe, if two threads run this at the same time, they might read the same count
      _ <- printTicks // recursive call
    } yield ()

    for {
      _ <- (tickingClock, printTicks).parTupled // run both IOs in parallel
    } yield ()
  }

  def tickingClockPure(): IO[Unit] = {
    val atomicTicks: IO[Ref[IO, Long]] = Ref[IO].of(0L)

    def tickingClock(ref: Ref[IO, Long]): IO[Unit] = for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug
      _ <- ref.update(ticks => ticks + 1) // this is thread-safe, it will atomically update the count
      _ <- tickingClock(ref) // recursive call
    } yield ()

    def printTicks(ref: Ref[IO, Long]): IO[Unit] = for {
      _ <- IO.sleep(5.seconds)
      ticks <- ref.get // this is thread-safe, it will atomically read the count
      _ <- IO(s"Ticks: $ticks").debug // this is thread-safe, it will atomically read the count
      _ <- printTicks(ref) // recursive call
    } yield ()

    for {
      ref <- atomicTicks // create the Ref
      _ <- (tickingClock(ref), printTicks(ref)).parTupled // run both IOs in parallel
    } yield ()

  }


  override def run = demoConcurrentWorkPure()

}
