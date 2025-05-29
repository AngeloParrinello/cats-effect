package com.rockthejvm.part2effects

import cats.Traverse
import cats.effect.{IO, IOApp}

import scala.concurrent.Future

object IOTraversal extends IOApp.Simple {
  
  import scala.concurrent.ExecutionContext.Implicits.global

  def heavyComputation(string: String): Future[Int] = Future {
    Thread.sleep(1000)
    string.length
  }
  val workload = List("I", "love", "cats")
  val listTraverse = Traverse[List]

  def clunkyFutures(): Unit = {
    val futures: List[Future[Int]] = workload.map(heavyComputation)
    // Future[List[Int]] would be hard to obtain
    futures.foreach(_.foreach(println))
  }

  def traverseFutures(): Unit = {
    //traverse
    import cats.Traverse
    import cats.instances.list.*

    val singleFuture: Future[List[Int]] = listTraverse.traverse(workload)(heavyComputation)
    // ^^ this stores ALL the results of the futures in a single Future
    singleFuture.foreach(println)
  }

  import com.rockthejvm.utils._
  def computeAsIO(string: String): IO[Int] = IO {
    Thread.sleep(1000)
    string.split(" ").length
  }.debug

  val ios: List[IO[Int]] = workload.map(computeAsIO)
  val singleIO: IO[List[Int]] = listTraverse.traverse(workload)(computeAsIO)

  // parallel traversal
  import cats.syntax.parallel._
  val parallelSingleIO: IO[List[Int]] = workload.parTraverse(computeAsIO)

  /**
   *
   * Exercises
   */
  // hint: use traverse API
  import cats._
  import cats.syntax.traverse._
  def sequence[A](listOfIOs: List[IO[A]]): IO[List[A]] = listTraverse.traverse(listOfIOs)(identity) // x => x

  def sequenceGeneral[F[_] : Traverse, A](generalIOs: F[IO[A]]): IO[F[A]] =
    Traverse[F].traverse(generalIOs)(identity)
    // generalIOs.traverse(identity)

  // hint: use parallel API
  def parSequence[A](listOfIOs: List[IO[A]]): IO[List[A]] = listOfIOs.parTraverse(identity)

  def parSequenceGeneral[F[_] : Traverse, A](generalIOs: F[IO[A]]): IO[F[A]] = generalIOs.parTraverse(identity)

  // existing sequence API
  val singleIOV2: IO[List[Int]] = listTraverse.sequence(ios)

  // existing parallel sequence API
  val parallelSingleIOV2: IO[List[Int]] = parSequence(ios) // same as ios.parTraverse(identity)
  val parallelSingleIOV3: IO[List[Int]] = ios.parSequence // extension method from the Parallel syntax package

  override def run: IO[Unit] = parallelSingleIO.map(_.sum).debug.void

}
