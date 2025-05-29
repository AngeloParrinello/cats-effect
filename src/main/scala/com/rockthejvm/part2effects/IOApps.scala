package com.rockthejvm.part2effects

import cats.effect.{ExitCode, IO, IOApp}

object IOApps {
  val program = for {
    _ <- IO.println("What's your name?")
    name <- IO.readLine
    _ <- IO.println(s"Hello, $name!")
  } yield ()
}

object TestApp {
  import IOApps.program

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global
    program.unsafeRunSync()
  }
}

// this pattern above is very common in Cats Effects
// and so they created a trait for this situation

object FirstCatsEffectApp extends IOApp {
  import IOApps.program
  override def run(args: List[String]): IO[ExitCode] = program.as(ExitCode.Success) // program.map(_ => ExitCode.Success)
}

object MySimpleApp extends IOApp.Simple {
  import IOApps.program
  override def run: IO[Unit] = program // returns always ExitCode.Success
}