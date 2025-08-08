package com.rockthejvm.part4coordination

import cats.effect.*
import cats.effect.std.CountDownLatch
import cats.syntax.all.*
import com.rockthejvm.part4coordination.CountDownLatches.FileServer
import com.rockthejvm.utils.*

import java.io.{File, FileWriter}
import scala.concurrent.duration.*
import scala.io.Source
import scala.util.Random

object CountDownLatches extends IOApp.Simple {

  /*
  CDLatches are a coordination primitive initialized with a count.
  All fibers calling await() on the CDLatch are (semantically) blocked until the count reaches 0.
  When the count reaches 0, all fibers waiting on the latch are released.
   */

  def announcer(latch: CountDownLatch[IO]): IO[Unit] = for {
    _ <- IO("Starting race shortly...").debug >> IO.sleep(2.seconds)
    _ <- IO("5...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("4...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("3...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("2...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("1...").debug >> IO.sleep(1.second)
    _ <- latch.release
    _ <- IO("GO!").debug
  } yield ()

  def createRunner(id: Int, latch: CountDownLatch[IO]): IO[Unit] = for {
    _ <- IO(s"Runner $id waiting for signal...").debug
    _ <- latch.await // this will block the fiber until the latch count reaches 0
    _ <- IO(s"Runner $id started running!").debug
  } yield ()

  def sprint(): IO[Unit] = {
    for {
      latch <- CountDownLatch[IO](5) // create a latch with a count of 5
      announcerFib <- announcer(latch).start // trigger the latch
      _ <- (1 to 10).toList.parTraverse(id => createRunner(id, latch)) // start 10 runners in parallel
      _ <- announcerFib.join // wait for the announcer to finish
    } yield ()
  }

  /**
   * Exercise: simulate a file downloader on multiple threads
   */

  object FileServer {
    val fileChunksList = List(
      "I love scala",
      "I love cats-effect",
      "I love concurrency"
    )

    def getNumChunks: IO[Int] = IO(fileChunksList.length)

    def getFileChunk(n: Int): IO[String] = IO(fileChunksList(n))
  }

  def writeToFile(path: String, content: String): IO[Unit] = {
    val fileResource = Resource.make(IO(new FileWriter(new File(path))))(writer => IO(writer.close()))
    fileResource.use {
      writer => IO(writer.write(content))
    }
  }

  def appendFileContents(fromPath: String, toPath: String): IO[Unit] = {
    val compositeResource = for {
      reader <- Resource.make(IO(Source.fromFile(new File(fromPath))))(reader => IO(reader.close()))
      writer <- Resource.make(IO(new FileWriter(new File(toPath), true)))(writer => IO(writer.close()))
    } yield (reader, writer)

    compositeResource.use {
      case (reader, writer) =>
        IO(reader.getLines().foreach(writer.write))
    }
  }

  /*
  - call file server API and get the number of chunks (n)
  - start a CDLatch
  - start n fibers which download a chunk of the file (use the file server's download chunk API)
  - block on  the latch until all chunks are downloaded
  - after all chunks are done, stitch the files together under the same file on disk
   */
  def downloadFile(fileName: String, destFolder: String): IO[Unit] =
    for {
      numChunks <- FileServer.getNumChunks
      latch <- CountDownLatch[IO](numChunks)
      _ <- (0 until numChunks).toList.parTraverse(id => downloadAsync(id, latch, fileName, destFolder))
      _ <- latch.await
      _ <- stitchFiles(fileName, destFolder)
      _ <- IO(s"File $fileName downloaded and stitched in $destFolder").debug
    } yield ()

  private def stitchFiles(fileName: String, destFolder: String): IO[Unit] =
    for {
      tempFiles <- IO(FileServer.fileChunksList.indices.map(id => s"$destFolder/$fileName-$id.txt").toList)
      _ <- tempFiles.traverse(file => appendFileContents(file, s"$destFolder/$fileName.txt"))
      _ <- tempFiles.traverse(file => IO(new File(file).delete()))
    } yield ()

  private def downloadAsync(chunkId: Int, latch: CountDownLatch[IO], tempFilename: String, tempDestFolder: String): IO[Unit] = {
    for {
      chunk <- FileServer.getFileChunk(chunkId)
      _ <- IO(s"Downloaded chunk $chunkId: $chunk").debug
      _ <- writeToFile(s"$tempDestFolder/$tempFilename-$chunkId.txt", chunk)
      _ <- IO(s"Chunk $chunkId written to file").debug
      _ <- latch.release
    } yield ()
  }

  override def run =
    downloadFile("myScalafile.txt", "cats-effect/src/main/resources")
}

/**
 * Exercise: Implement your own CountDownLatch with Ref and Deferred
 */
trait MyCountDownLatch {
  def await: IO[Unit]

  def release: IO[Unit]
}

object MyCountDownLatch {
  sealed trait InternalState

  case object Done extends InternalState

  case class Live(countLeft: Int, signal: Deferred[IO, Unit]) extends InternalState


  def apply(count: Int): IO[MyCountDownLatch] =
    for {
      signal <- Deferred[IO, Unit]
      state <- Ref[IO].of[InternalState](Live(count, signal))
    } yield new MyCountDownLatch {
      override def await: IO[Unit] =
        state.get.flatMap {
          case Done => IO.unit
          case Live(_, signal) => signal.get // block on the signal
        }

      override def release: IO[Unit] =
        state.modify {
          case Done => (Done, IO.unit)
          case Live(1, signal) =>
            (Done, signal.complete(()).void)
          case Live(countLeft, signal) =>
            (Live(countLeft - 1, signal), IO.unit)
        }.flatten
    }
}





