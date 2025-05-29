package com.rockthejvm.part2effects

import cats.effect.IO

object IOIntroduction extends App {

  // IO: embodies the idea of any type of computation that can produce side-effects
  val ourFirstIo: IO[Int] = IO.pure(42) // pure takes an arg that shoudl not have side effects
  val aDelayedIO: IO[Int] = IO.delay({
    println("Starting the IO")
    42
  })

  // this will be printed
  val shouldNotDoThis = IO.pure {
    println("This is a side effect")
    42
  }

  val aDelayedIoV2 = IO { // apply == delay
    println("Starting the IO")
    42
  }

  // IO is very similar to the case class that we produced on the previous lecture but with many more capabilities
  import cats.effect.unsafe.implicits.global // import the global execution context, the "platform" where the IO will be executed
  println(aDelayedIO.unsafeRunSync())

  // so the main goal for us programmers is compress and compose all these IOs
  // map, flatmap
  val improvedMeaningOfLife = aDelayedIO.map(_ * 2)
  val theMeaningOfLife = aDelayedIO.flatMap(value => IO(value * 2))

  def smallProgram(): IO[Unit] = for {
    _ <- IO.println("What's your name?")
    name <- IO.readLine
    _ <- IO.println(s"Hello, $name!")
  } yield ()

  smallProgram().unsafeRunSync()

  // mapN - to combine IO effects to tuples
  import cats.syntax.apply._
  val combinedMeaningOfLife: IO[Int] = (aDelayedIO, aDelayedIO).mapN(_ + _)
  def smallProgramV2(): IO[Unit] = (IO.println("What's your name?"), IO.readLine).mapN((_, name) => s"Hello, $name!").flatMap(IO.println)

  smallProgramV2().unsafeRunSync()

  /**
   * Exercises
   */

  // 1 - sequence two IOs and take the result of the second one
  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] = ioa.flatMap(_ => iob)

  def sequenceTakeLastV2[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa *> iob // the *> operator is the andThen operator!

  def sequenceTakeLastV3[A, B](ioa: IO[A], iob: IO[B]): IO[B] =
    ioa >> iob // the >> operator is the andThen operator (so basically identical to the previous one), but with lazy evaluation!

  // 2 - sequence two IOs and take the result of the first one
  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa.flatMap(a => iob.map(_ => a))

  def sequenceTakeFirstV2[A, B](ioa: IO[A], iob: IO[B]): IO[A] =
    ioa <* iob // the <* operator is the andThen and take first operator!

  // 3 - repeat an IO forever
  // hint:use flatmap and recursion
  // flatMap in IO is stack safe (use a tail recursive algorithm), so we can use it for recursion
  def forever[A](io: IO[A]): IO[A] =
    io.flatMap(_ => forever(io))

  // this will create an infinite linked chain in which each node will be evaluated lazily
  // as the flatMap!
  def foreverV2[A](io: IO[A]): IO[A] =
    io >> foreverV2(io) // same as the previous one, but with lazy evaluation

  // but this will end in a stack overflow error! because io is evaluated eagerly
  // and the recursive call is evaluated eagerly as well
  // often better to use the >> operator instead of this one
  def foreverV3[A](io: IO[A]): IO[A] =
    io *> foreverV3(io) // same as the previous one, but with eager evaluation

  def foreverV4[A](io: IO[A]): IO[A] =
    io.foreverM // same as the previous one, but with lazy evaluation so tail recursive and stack safe


  // 4: convert an IO to a different type
  def convert[A, B](ioa: IO[A], value: B): IO[B] =
    ioa.flatMap(_ => IO.pure[B](value))

  def convertV2[A, B](ioa: IO[A], value: B): IO[B] =
    ioa.map(_ => value)

  def convertV3[A, B](ioa: IO[A], value: B): IO[B] =
    ioa.as(value) // exact same as the previous one

  // 5: discard value inside an IO just return Unit
  def asUnit[A](ioa: IO[A]): IO[Unit] =
    ioa.flatMap(_ => IO(()))

  def asUnitV2[A](ioa: IO[A]): IO[Unit] =
    ioa.as(()) // exact same as the previous one

  def asUnitV3[A](ioa: IO[A]): IO[Unit] =
    ioa.void // exact same as the previous one

  def asUnitV4[A](ioa: IO[A]): IO[Unit] =
    ioa.map(_ => ()) // exact same as the previous one

  // 6: fix stack recursion
  def sum(n: Int): Int =
    if (n <= 0) 0
    else n + sum(n - 1)

  def sumIO(n: Int): IO[Int] = // this will crash with a stack overflow error
    if (n <= 0) IO.pure(0) // IO(0)
    else (IO.pure(n), sumIO(n -1)).mapN(_ + _)

  def sumIOV2(n: Int): IO[Int] =
    if (n <= 0) IO.pure(0) // IO(0)
    else for {
      lastNumber <- IO(n)
      prevSum <- sumIOV2(n - 1)
    } yield lastNumber + prevSum

  // 7: write a fibonacci function IO that does not crash on recursion
  // hint: use recursion, ignore exponential complexity and use flatmap heavily
  def fibonacci(n: Int): IO[Int] = // this will crash with a stack overflow error
    if (n <= 1) IO.pure(1)
    else (fibonacci(n - 1), fibonacci(n - 2)).mapN(_ + _)

  def fibonacciV2(n: Int): IO[Int] =
    if (n <= 1) IO.pure(1)
    else for {
      last <- IO(fibonacciV2(n - 1)).flatMap(x => x)
      prev <- IO(fibonacciV2(n - 2)).flatMap(x => x)
    } yield last + prev

  def fibonacciV3(n: Int): IO[Int] =
    if (n <= 1) IO.pure(1)
    else for {
      last <- IO(fibonacciV3(n - 1)).flatten // same as above
      prev <- IO(fibonacciV3(n - 2)).flatten
    } yield last + prev

  def fibonacciV4(n: Int): IO[Int] =
    if (n <= 1) IO.pure(1)
    else for {
      last <- IO.defer(fibonacciV4(n - 1)) // since IO.delay(fibonacciV3(n - 1)).flatten is the same as IO(fibonacciV3(n - 1)).flatten, we can re-write that in this way
      prev <- IO.defer(fibonacciV4(n - 2)) // we are basically suspending the evaluation of the IO, is the same as .delay(...).flatten
    } yield last + prev


}
