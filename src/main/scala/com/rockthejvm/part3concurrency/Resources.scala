package com.rockthejvm.part3concurrency

import cats.effect.kernel.Outcome.{Canceled, Errored, Succeeded}
import cats.effect.kernel.Resource.ExitCase
import cats.effect.{IO, IOApp, Resource}

import java.io.{File, FileReader}
import java.util.Scanner
import scala.concurrent.duration.*

object Resources extends IOApp.Simple:
  import com.rockthejvm.utils._

  // use-case: manage a connection lifecycle
  class Connection(url: String) {
    def open: IO[String] = IO(s"opening connection to $url").debug
    def close: IO[String] = IO(s"closing connection to $url").debug
  }

  // resource: a connection that can be opened and closed

  // what's the problem with the following code?
  // - the connection is opened, but never closed
  // you are actually leaking resources, while you are canceling the fiber, you are not closing the connection
  val asyncFetchUrl = for {
    fib <- (new Connection("rockthejvm.com").open *> IO.sleep(Int.MaxValue.seconds)).start
    _ <- IO.sleep(1.second) *> fib.cancel // cancel the fiber after 1 second
  } yield ()

  // here we are using and then closing the connection
  // but it's very difficult to manage the lifecycle of the connection! and so a bit unreadable
  val correctAsyncFetchUrl = for {
    conn <- IO(new Connection("rockthejvm.com"))
    fib <- (conn.open *> IO.sleep(Int.MaxValue.seconds)).onCancel(conn.close.void).start
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()

  // cats provides a better way to manage resources
  // the bracket pattern
  /*
  someIO.bracket(useResourceCallback)(releaseResourceCallback)
  bracket is equivalent to try-catches but in pure FP
    */
  val bracketFetchUrl = IO(new Connection("rockthejvm.com"))
    //           open the connection and do something with it         close it
    .bracket(conn => conn.open *> IO.sleep(Int.MaxValue.seconds))(conn => conn.close.void)

  val bracketProgram = for {
    fib <- bracketFetchUrl.start
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()

  /**
   * Exercise: read the file with the bracket pattern
   * - open a scanner
   * - read the file line by line, every 100 millis
   * - close the scanner
   * - if cancelled/throws error, close the scanner
   */
  def openFileScanner(path: String): IO[Scanner] = {
    IO(new Scanner(new FileReader(new File(path))))
  }

  def readLineByLine(scanner: Scanner): IO[Unit] =
    if (scanner.hasNextLine) {
      IO(println(scanner.nextLine())) >> IO.sleep(100.millis) >> readLineByLine(scanner)
    } else {
      IO.unit
    }

  def bracketReadFile(path: String): IO[Unit] =
    IO(s"Opening file at $path") >>
      openFileScanner(path).bracket {
        scanner =>
          readLineByLine(scanner)
      } {
        scanner =>
          IO(s"Closing file at $path") >> IO(scanner.close())
      }
//    openFileScanner(path)
//      .bracket(scanner => IO(scanner.close())) { scanner =>
//        IO {
//          while (scanner.hasNextLine) {
//            println(scanner.nextLine())
//            Thread.sleep(100)
//          }
//        }
//      }


  /**
   * Resources!!!
   */
  def connFromConfig(path: String): IO[Unit] =
    openFileScanner(path)
      .bracket{
        scanner =>
          // acquire a connection based on the file
          IO(new Connection(scanner.nextLine())).bracket {
            conn =>
              // use the connection
              conn.open.debug >> IO.never
          }(conn => conn.close.void)
      }(scanner => IO("closing file").debug >> IO(scanner.close()))

    // but at this point our code is not so readable anymore...because nesting resources are tedious

    // we can use the Resource type from cats-effect

  val connectionResource = Resource.make(IO(new Connection("rockthejvm.com")))(conn => conn.close.void)
  // so at the moment we have defined a resource and its way to handle the closure
  //... at a later part of my code, later on in the execution...
  val resourceFetchUrl = for {
    fib <- connectionResource.use(conn=> conn.open >> IO.never).start
    _ <- IO.sleep(1.second) >> fib.cancel
  } yield ()


  // resources are equivalent to brackets
  val simpleResource = IO("some resource")
  val usingResource: String => IO[String] = string => IO(s"using the resource $string").debug
  val releaseResource: String => IO[Unit] = string => IO(s"releasing the resource $string").debug.void

  val usingResourceWithBracket = simpleResource.bracket(usingResource)(releaseResource)
  val usingResourceWithResource = Resource.make(simpleResource)(releaseResource).use(usingResource)


  /**
   * Exercise: read a text fle with one line every 100 millis using Resource
   * hint: refactor the bracket pattern to use Resource
   */
  def readFileResource(path: String): IO[Unit] =
    val scanner = Resource.make(openFileScanner(path))(scanner => IO(s"Closing file at $path").debug >> IO(scanner.close()))
    IO(s"Opening file at $path") >>
      scanner.use {
        scanner =>
          readLineByLine(scanner)
      }

  def cancelReadFile(path: String) = for {
    fib <- readFileResource(path).start
    _ <- IO.sleep(2.second) >> fib.cancel
  } yield ()

  // let's get back to the original problem, the nested resources
  def connFromConfigResource(path: String): Resource[IO, Connection] =
    Resource.make(openFileScanner(path))(scanner => IO(s"Closing file at $path").debug >> IO(scanner.close()))
      .flatMap(scanner => Resource.make(IO(new Connection(scanner.nextLine())))(conn => conn.close.void))

  val openConnection =
    connFromConfigResource("src/main/resources/connection.txt")
      .use(conn => conn.open >> IO.never)

  val canceledConnection = for {
    fib <- openConnection.start
    _ <- IO.sleep(2.second) >> fib.cancel
  } yield ()

  // connection + file will close automatically

  // but using flatmap does make our code a lot more readable
  // BUT we can use for-comprehensions to make it more readable
  def connFromConfigResource2(path: String): Resource[IO, Connection] =
    for {
      scanner <- Resource.make(openFileScanner(path))(scanner => IO(s"Closing file at $path").debug >> IO(scanner.close()))
      conn <- Resource.make(IO(new Connection(scanner.nextLine())))(conn => conn.close.void)
    } yield conn

  // finalizers to regular IOs (to regular effects)
  // finalizers are the equivalent of the release function in the bracket pattern
  val ioWithFinalizer = IO("some resource").debug.guarantee(IO("freeing the resource").debug.void)
  val ioWithFinalizerV2 = IO("some resource").debug.guaranteeCase {
    case Succeeded(fa) => fa.flatMap(result => IO(s"freeing the resource with result $result").debug.void)
    case Errored(e) => IO(s"freeing the resource with error $e").debug.void
    case Canceled() => IO("freeing the resource with cancel").debug.void
  }


  override def run: IO[Unit] = ioWithFinalizer.void
//canceledConnection
//openConnection.void
//cancelReadFile("src/main/scala/com/rockthejvm/part3concurrency/Resources.scala")
