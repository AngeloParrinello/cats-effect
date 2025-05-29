package com.rockthejvm.part3concurrency

import cats.effect.kernel.Fiber
import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.{IO, IOApp}

import scala.concurrent.duration.*

object CancellingIOs extends IOApp.Simple {

  import com.rockthejvm.utils.*

  /*
  Cancelling IOs
  - fib.cancel
  - IO.race & other APIs
  - manual cancellation
   */
  val chainOfIOs = IO("waiting").debug >> IO.canceled >> IO(42).debug

  // uncancelable IOs
  // example: online store, payment processing, it should never be cancelled
  // we can call cancel to that IO but should not cancel it
  val specialPaymentSystem = (
    IO("processing payment").debug >>
      IO.sleep(10.seconds) >>
      IO("payment processed").debug
    ).onCancel(IO("Mega cancel of doom!").debug.void)

  val cancellationOfDoom = for {
    fib <- specialPaymentSystem.start
    _ <- IO.sleep(1.second) >> fib.cancel
    _ <- fib.join
  } yield ()

  // uncancelable IOs
  // uncancelable IOs are not cancelled by the cancelation of the parent IO
  // they are "masked" from the cancelation
  // the cats effect runtime will not cancel the uncancelable IOs
  val atomicPayment = IO.uncancelable(_ => specialPaymentSystem) // "masking" the cancelation of the IO
  val atomicPaymentV2 = specialPaymentSystem.uncancelable // same as above, but more readable

  val noCancellationOfDoom = for {
    fib <- atomicPayment.start
    _ <- IO.sleep(1.second) >> IO("Attempting cancellation...").debug >> fib.cancel // this will not cancel the atomicPayment
    _ <- fib.join // this will wait for the atomicPayment to finish
  } yield ()

  /*
   The uncancelable API is more complex and general.
  It takes a function from Poll[IO] to IO. IN the example above, we aren't using that Poll instance.
  THe Poll object can be used to mark sections within the returned effect which CAN BE CANCELED.
   */

  // Example: authehtnication service, has two parts:
  // - input password: can be cancelled otherwise we might block indefinitely on user input
  // - checking password: should not be cancelled, otherwise we might leave the system in an inconsistent state

  val inputPassword = IO("Input password").debug >> IO("Typing password...").debug >> IO.sleep(5.seconds) >> IO("Password typed").debug
  val checkPassword = (pw: String) => IO("Checking password").debug >> IO.sleep(10.seconds) >> IO(pw == "secret").debug

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

  val authFlowPartiallyCancelable = IO.uncancelable {
    poll =>
      for {
        // Poll basically "unmask" the cancelation of the inputPassword
        password <- poll(inputPassword).onCancel(IO("Authentication cancelled").debug.void) // this is NOW cancellable
        isValid <- checkPassword(password) // this cannot be cancelled
        _ <- if (isValid) IO("Authentication successful").debug else IO("Authentication failed").debug
      } yield ()
  }

  val authProgramPartiallyCancelable = for {
    fib <- authFlowPartiallyCancelable.start
    _ <- IO.sleep(1.second) >> IO("Attempting cancellation...").debug >> fib.cancel // this will cancel the inputPassword, but not the checkPassword
    _ <- fib.join // this will wait for the authFlow to finish
  } yield ()

  /*
  Uncancelable calls are MASKS which suppress cancellation.
  Polls calls are "gaps opened" in the uncancellable region.
   */

  // exercise 1
  val cancelBeforeMol = IO.canceled >> IO(42).debug
  val uncancelableMol = IO.uncancelable(_ => cancelBeforeMol)
  // What happens if we run this program above?
  // The IO.canceled will be eliminated by the uncancelable and 42 will be printed

  // exercise 2
  val invincibleAuthProgram = for {
    fib <- IO.uncancelable(_ => authFlowPartiallyCancelable).start
    _ <- IO.sleep(1.second) >> IO("Attempting cancellation...").debug >> fib.cancel // this will cancel the inputPassword, but not the checkPassword
    _ <- fib.join // this will wait for the authFlow to finish
  } yield ()
  // What happens if we run this program above?
  // The authFlowPartiallyCancelable will not be cancelled, because it's now uncancelable!
  // the fib.cancel will be completely ignored

  // Exercise 3
  def threeStepProgram(): IO[Unit] = {
    val sequence = IO.uncancelable { poll =>
      poll(IO("Cancelable").debug >> IO.sleep(1.second)) >>
      IO("Uncancelable").debug >> IO.sleep(1.second) >>
      poll(IO("Cancelable again").debug >> IO.sleep(1.second))
    }

    for {
      fib <- sequence.start
      _ <- IO.sleep(1500.millis) >> IO("Attempting cancellation...").debug >> fib.cancel // this will cancel the first and third step, but not the second
      _ <- fib.join // this will wait for the sequence to finish
    } yield ()
  }

  // what happens if we run this program above?
  // At first glance, it seems like the first and third steps will be cancelled, but the second step will not.
  // However, the .cancel method will be interpreted by the first cancellable region! So the print
  // "Cancelable again" will not be printed, because the cancelation will be propagated to the first cancellable region (if you have one!).




  override def run: IO[Unit] = threeStepProgram()

}
