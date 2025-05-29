package com.rockthejvm.part2effects

import cats.effect.IO

import scala.util.{Failure, Success, Try}

object IoErrorHandling extends App {

  // IO: pure, delay, defer
  // create failed IOs, effects
  val aFailedCompute: IO[Int] = IO.delay(throw new RuntimeException("Failed computation"))

  import cats.effect.unsafe.implicits.global
  // this will throw the error but the line above not
  // aFailedCompute.unsafeRunSync()

  val aFailure: IO[Int] = IO.raiseError(new RuntimeException("Failed computation"))
  // aFailure.unsafeRunSync()

  // either the way are fine if you want to throw an error with IO BUT the second one is more idiomatic and readable

  // given the facts that we have a failed IO, we can handle it
  val dealWithIt = aFailure.handleErrorWith {
    case e: RuntimeException => IO(0)
  }
  // no error will be thrown, but 0 will be printed
  dealWithIt.unsafeRunSync()

  // turn into an Either
  val effectAsEither: IO[Either[Throwable, Int]] = aFailure.attempt

  // redeem: transform the failure and the success in one go
  // useful for transform both the success and the failure in one go into a String or something else
  val resultAsString: IO[String] = aFailure.redeem(ex => s"Failed with $ex", value => s"Got the result: $value")
  println(resultAsString.unsafeRunSync())

  // redeemWith: transform the failure and the success in one go, but the transformation is itself an IO
  val anotherFailure: IO[Unit] = aFailure.redeemWith(ex => IO(println(s"Failed with $ex")), value => IO(println(s"Got the result: $value")))
  anotherFailure.unsafeRunSync()

  /**
   * Exercises
   */

  //1) construct potentially failed IOs from standard types (Option, Try, Either)
  def option2IO[A](option: Option[A])(ifEmpty: Throwable): IO[A] = option match {
    case Some(x) => IO(x)
    case None => IO.raiseError(ifEmpty)
  }

  def try2IO[A](tryValue: Try[A]): IO[A] = tryValue match {
    case Success(value) => IO(value)
    case Failure(exception) => IO.raiseError(exception)
  }

  def either2IO[A](either: Either[Throwable, A]): IO[A] = either match {
    case Right(value) => IO(value)
    case Left(value) => IO.raiseError(value)
  }

  //2) create handleError and handleErrorWith for IO
  def handleIOError[A](io: IO[A])(handler: Throwable => A): IO[A] = io.redeem(handler, identity)
  def handleIOErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] = io.redeemWith(handler, IO(_))

}
