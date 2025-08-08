package com.rockthejvm.part5polymorphic

import cats.effect.kernel.Outcome.Succeeded
import cats.effect.{IO, IOApp, MonadCancel, Poll}
import cats.syntax.all.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.{Applicative, Monad}
import com.rockthejvm.utils.*

import scala.concurrent.duration.*


object PolymorphicCancellation extends IOApp.Simple {

  trait MyApplicativeError[F[_], E] extends cats.Applicative[F] {
    def raiseError[A](e: E): F[A]

    def handleErrorWith[A](fa: F[A])(f: E => F[A]): F[A]
  }

  trait MyMonadError[F[_], E] extends MyApplicativeError[F, E] with cats.Monad[F]

  // First CE type class
  // MonadCancel
  trait MyPoll[F[_]] {
    def apply[A](fa: F[A]): F[A]
  }

  trait MyMonadCancel[F[_], E] extends MyMonadError[F, E] {
    def canceled: F[Unit]

    def uncancelable[A](poll: Poll[F] => F[A]): F[A]
  }

  // monadCancel for IO
  val monadCancelIO: MonadCancel[IO, Throwable] = MonadCancel[IO]
  val molIO: IO[Int] = monadCancelIO.pure(42)
  val ambitiousMolIO: IO[Int] = monadCancelIO.map(molIO)(_ + 1)

  val mustCompute = monadCancelIO.uncancelable { _ =>
    for {
      _ <- monadCancelIO.pure(println("Once started, I can't go back!"))
      res <- monadCancelIO.pure(42)
    } yield res
  }

  def mustComputeGeneral[F[_], E](implicit
                                  monadCancel: MonadCancel[F, E]
                                 ): F[Int] = {
    monadCancel.uncancelable { _ =>
      for {
        _ <- monadCancel.pure(println("Once started, I can't go back!"))
        res <- monadCancel.pure(42)
      } yield res
    }
  }

  val mustComputeV2 = mustComputeGeneral[IO, Throwable]

  // allow cancellation listeners
  val mustComputeWithListener = mustCompute.onCancel(IO("Cancelled!").void)
  val mustComputeWithListenerV2 = monadCancelIO.onCancel(mustCompute, IO("Cancelled!").void) // same as above

  // .onCancel as extension method

  import cats.effect.syntax.monadCancel.*

  // allow finalizers
  val aComputationWithFinalizer = monadCancelIO.guaranteeCase(IO(42)) {
    case Succeeded(fa) => fa.flatMap(result => IO(s"Computation succeeded with result: $result").void)
    case _ => IO("Computation failed or was cancelled").void
  }

  // bracket pattern is specific to MonadCancel
  val aComputationWithUsage = monadCancelIO.bracket(
    IO(42)
  )(
    result => IO(s"Using result: $result").void
  )(
    _ => IO("Finalizing...").void
  )



  // Exercise - Generalize a piece of code

  import com.rockthejvm.utils.general.*

  // to use instead of IO.sleep for this exercise
  def unsafeSleep[F[_], E](duration: FiniteDuration)(implicit
                                                     monadCancel: MonadCancel[F, E]
  ): F[Unit] = {
    monadCancel.pure {
      Thread.sleep(duration.toMillis) // not semantic blocking
    }
  }

  def inputPassword[F[_], E](implicit
                             monadCancel: MonadCancel[F, E]
                            ): F[String] = {
    monadCancel.pure("Input password").debug >>
      monadCancel.pure("Typing password...").debug >>
      unsafeSleep[F, E](5.seconds) >>
      monadCancel.pure("Password typed").debug
  }

  val inputPassword: IO[String] = IO("Input password").debug >> IO("Typing password...").debug >> IO.sleep(5.seconds) >> IO("Password typed").debug

  def checkPassword[F[_], E](pw: String)(implicit
                                         monadCancel: MonadCancel[F, E]
  ): F[Boolean] = {
    monadCancel.pure("Checking password").debug >>
      unsafeSleep[F, E](10.seconds) >>
      monadCancel.pure(pw == "secret").debug
  }

  val checkPassword = (pw: String) => IO("Checking password").debug >> IO.sleep(10.seconds) >> IO(pw == "secret").debug

  // and the same for the authentication flow...
  // def authFlow[F[_], E](implicit...

  val authFlowUncancelable = IO.uncancelable {
    poll =>
      for {
        password <- inputPassword.onCancel(IO("Authentication cancelled").debug.void) // this can be cancelled
        isValid <- checkPassword(password) // this cannot be cancelled
        _ <- if (isValid) IO("Authentication successful").debug else IO("Authentication failed").debug
      } yield ()
  }

  val authProgram = for {
    fib <- authFlowUncancelable.start
    _ <- IO.sleep(1.second) >> IO("Attempting cancellation...").debug >> fib.cancel // this will cancel the inputPassword, but not the checkPassword
    _ <- fib.join // this will wait for the authFlow to finish
  } yield ()


  override def run = ???

}
