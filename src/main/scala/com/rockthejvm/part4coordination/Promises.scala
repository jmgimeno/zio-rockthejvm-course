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

  /** Exercises:
    */

  /**   1. Write a simulated "egg boiler" with two ZIOs
    *      - one increments a counter every second
    *      - one waits for the counter to become 10, after which it will 'ring a bell'
    */

  def eggBoiler_JM(): UIO[Unit] =
    def incrementer(counter: Ref[Int], finished: Promise[Nothing, Unit]): UIO[Unit] =
      for
        count <- counter.updateAndGet(_ + 1)
        _ <-
          if (count == 10)
          then finished.succeed(())
          else
            ZIO.succeed("sleeping...").debugThread
              *> ZIO.sleep(1.second)
              *> incrementer(counter, finished)
      yield ()

    for
      counter  <- Ref.make(0)
      finished <- Promise.make[Nothing, Unit]
      _        <- ZIO.succeed("start boiling...").debugThread
      _        <- incrementer(counter, finished).fork
      _        <- finished.await
      _        <- ZIO.succeed("finished boiling").debugThread
    yield ()

  def eggBoiler_DAniel(): UIO[Unit] =
    def eggReady(signal: Promise[Nothing, Unit]) =
      for
        _ <- ZIO.succeed("Egg boiling on some other fiber, waiting...").debugThread
        _ <- signal.await
        _ <- ZIO.succeed("EGG READY!").debugThread
      yield ()

    def tickingClock(ticks: Ref[Int], signal: Promise[Nothing, Unit]): UIO[Unit] =
      for
        _     <- ZIO.sleep(1.second)
        count <- ticks.updateAndGet(_ + 1)
        _     <- ZIO.succeed(count).debugThread
        _     <- if (count == 10) then signal.succeed(()) else tickingClock(ticks, signal)
      yield ()

    for
      ticks  <- Ref.make(0)
      signal <- Promise.make[Nothing, Unit]
      _      <- eggReady(signal) zipPar tickingClock(ticks, signal)
    yield ()

  /** 2.Write a "racePair"
    *   - use a Promise which can hold an Either[exit for A, exit for B]
    *   - start a fiber for each ZIO
    *   - on completion (with any status), each ZIP needs to complete that Promise (hint: use a
    *     finalizer)
    *   - waiting on the promise's value can be interrupted!
    *   - if the whole race is interrupted, interrupt the running fibers
    */

  def racePair[R, E, A, B](
      zioa: => ZIO[R, E, A],
      ziob: => ZIO[R, E, B]
  ): ZIO[R, E, Either[(Exit[E, A], Fiber[E, B]), (Fiber[E, A], Exit[E, B])]] =
    ZIO.uninterruptibleMask { restore =>
      for
        promise <- Promise.make[Nothing, Either[Exit[E, A], Exit[E, B]]]
        fibA    <- restore(zioa).onExit(exita => promise.succeed(Left(exita))).fork
        fibB    <- restore(ziob).onExit(exitb => promise.succeed(Right(exitb))).fork
        result <- restore(promise.await).onInterrupt {
          for
            interrupta <- fibA.interrupt.fork
            interruptb <- fibB.interrupt.fork
            _          <- interrupta.join
            _          <- interruptb.join
          yield ()
        }
      yield result match
        case Left(exita)  => Left((exita, fibB))
        case Right(exitb) => Right((fibA, exitb))
    }

  val demoRacePair =
    val zioa = ZIO
      .sleep(5.second)
      .as(1)
      .debugThread
      .onInterrupt(ZIO.succeed("first interrupted").debugThread)
    val ziob = ZIO
      .sleep(8.seconds)
      .as(2)
      .debugThread
      .onInterrupt(ZIO.succeed("second interrupted").debugThread)

    val pair = racePair(zioa, ziob)
    pair.flatMap {
      case Left((exita, fibb)) =>
        fibb.interrupt *> ZIO.succeed("first won").debugThread *> ZIO.succeed(exita).debugThread
      case Right((fiba, exitb)) =>
        fiba.interrupt *> ZIO.succeed("second won").debugThread *> ZIO.succeed(exitb).debugThread
    }

  def run = demoRacePair
