package com.rockthejvm.part2effects

import cats.Parallel
import cats.effect.{IO, IOApp}

object IOParallelism extends IOApp.Simple{

  // IOs are usually sequential: flatmap, map and so on, happen one after the other on the same thread
  val anisIO = IO(s"[${Thread.currentThread().getName}] Ani!")
  val mariaIO = IO(s"[${Thread.currentThread().getName}] Maria!")

  val composedIO = for {
    ani <- anisIO
     maria <- mariaIO
  } yield s"$ani, $maria love cats"
  // the above will run sequentially

  // debug extension method defined in Utils
  import com.rockthejvm.utils.*
  // mapN extension method
  import cats.syntax.apply.*
  val meaningOfFile = IO(42).debug
  val favLang = IO("Scala").debug
  val goAndLife = (meaningOfFile, favLang).mapN((meaning, lang) => s"Meaning of life is $meaning and my favorite language is $lang")


  // parallelism on IOs
  // convert a sequential IO to a parallel IO
  val parallelIO1: IO.Par[Int] = Parallel[IO].parallel(meaningOfFile.debug)
  val parallelIO2: IO.Par[String] = Parallel[IO].parallel(favLang.debug)
  import cats.effect.implicits.*
  val parallelMeaningAndLang: IO.Par[String] = (parallelIO1, parallelIO2).mapN((meaning, lang) => s"Meaning of life is $meaning and my favorite language is $lang")
  // turn back to sequential IO
  val goalInLifeV2: IO[String] = Parallel[IO].sequential(parallelMeaningAndLang)

    // this a very common pattern in Effect libraries
    // there is a shorthand
    // the parMapN extension method does the same thing as defined above from line 29 34
    import cats.syntax.parallel.*
    val goalInLifeV3 = (meaningOfFile.debug, favLang.debug).parMapN((meaning, lang) => s"Meaning of life is $meaning and my favorite language is $lang")

    // what happens if there is a failure?
    val aFailure = IO.raiseError[Int](new RuntimeException("Meaning of life not found"))
    val aSuccess = IO(42)
    val parallelWithFailure = (aSuccess.debug, aFailure.debug).parMapN(_ + _)
    // tldr: if any of the IOs in the parMapN fails, the whole thing fails

    // compose failure + failure
    val anotherFailure = IO.raiseError[Int](new RuntimeException("I have no idea what I'm doing"))
    val composedFailure = (aFailure.debug, anotherFailure.debug).parMapN(_ + _) // we do not know which error will be thrown!!
    // but if we delay one of the failures, we can see the other one first
    val composedFailureDelayed = (IO(Thread.sleep(1000)) >> aFailure.debug, anotherFailure.debug).parMapN(_ + _)
    // what does this mean? that the first IO that fails will be the one that will be thrown!!

  override def run: IO[Unit] =
    // composedIO.map(println)
    // goAndLife.map(println)
    // goalInLifeV2.map(println)
    // goalInLifeV2.debug.void
   // goalInLifeV3.debug.void
   // parallelWithFailure.debug.void
    // composedFailure.debug.void
    composedFailureDelayed.debug.void

}
