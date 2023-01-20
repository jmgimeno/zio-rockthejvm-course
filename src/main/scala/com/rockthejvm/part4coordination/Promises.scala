package com.rockthejvm.part4coordination

import zio.*
import com.rockthejvm.utils.*

object Promises extends ZIOAppDefault:

  val aPromise: UIO[Promise[Throwable, Int]] = Promise.make[Throwable, Int]

  // await - block the fiber until the promise has a value
  var reader = aPromise.flatMap { promise =>
    promise.await
  }

  // succeed, fail, complete
  // unblocks whatever awaits

  val writer = aPromise.flatMap { promise =>
    promise.succeed(42) // with value
  // promise.fail // with throwable
  // promise.complete // with an IO
  }

  def demoPromise(): Task[Unit] =
    // producer - consumer problem
    def consumer(promise: Promise[Throwable, Int]): Task[Unit] =
      for
        _   <- ZIO.succeed(s"[consumer] waiting for result...").debugThread
        mol <- promise.await
        _   <- ZIO.succeed(s"[consumer] I got the result $mol").debugThread
      yield ()

    def producer(promise: Promise[Throwable, Int]): UIO[Unit] =
      for
        _   <- ZIO.succeed(s"[producer] crunching numbers...").debugThread
        _   <- ZIO.sleep(3.seconds)
        _   <- ZIO.succeed(s"[producer] complete").debugThread
        mol <- ZIO.succeed(42)
        _   <- promise.succeed(mol)
      yield ()

    for
      promise <- Promise.make[Throwable, Int]
      _       <- consumer(promise) zipPar producer(promise)
    yield ()

    /*
      - purely functional block on a fiber until you get a signal from another fiber
      - waiting on a value which may not yet be available, without thread starvation
      - inter-fiber communication
     */

  // simulate downloading from multiple parts
  val fileParts = List("I ", "love S", "cala", " with pure FP an", "d ZIO! <EOF>")

  def downloadFileWithRef(): UIO[Unit] =
    def downloadFile(contentRef: Ref[String]): UIO[Unit] =
      ZIO.collectAllDiscard(
        fileParts.map { part =>
          ZIO.succeed(s"got: $part").debugThread
            *> ZIO.sleep(1.second)
            *> contentRef.update(_ + part)
        }
      )

    // with a ref we're busy-waiting to completion
    def notifyFileComplete(contentRef: Ref[String]): UIO[Unit] =
      for
        file <- contentRef.get
        _ <-
          if file.endsWith("<EOF>")
          then ZIO.succeed("file download complete.").debugThread
          else
            ZIO.succeed("downloading...").debugThread
              *> ZIO.sleep(500.millis)
              *> notifyFileComplete(contentRef)
      yield ()

    for
      contentRef <- Ref.make("")
      _          <- downloadFile(contentRef) zipPar notifyFileComplete(contentRef)
    yield ()

  def downloadFileWithRefPromise(): Task[Unit] =
    def downloadFile(contentRef: Ref[String], promise: Promise[Throwable, String]): UIO[Unit] =
      ZIO.collectAllDiscard(
        fileParts.map { part =>
          for
            _    <- ZIO.succeed(s"got: $part").debugThread
            _    <- ZIO.sleep(1.second)
            file <- contentRef.updateAndGet(_ + part)
            _ <-
              if file.endsWith("<EOF>")
              then promise.succeed(file)
              else ZIO.unit
          yield ()
        }
      )

    // with a ref we're busy-waiting to completion
    def notifyFileComplete(
        promise: Promise[Throwable, String]
    ): Task[Unit] =
      for
        _    <- ZIO.succeed("downloading...").debugThread
        file <- promise.await
        _    <- ZIO.succeed(s"file download complete: $file").debugThread
      yield ()

    for
      contentRef <- Ref.make("")
      promise    <- Promise.make[Throwable, String]
      _          <- downloadFile(contentRef, promise) zipPar notifyFileComplete(promise)
    yield ()
  def run = downloadFileWithRefPromise()
