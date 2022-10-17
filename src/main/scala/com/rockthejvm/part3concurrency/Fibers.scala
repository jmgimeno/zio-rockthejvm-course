package com.rockthejvm.part3concurrency

import zio.*
import com.rockthejvm.utils.*

object Fibers extends ZIOAppDefault:

  val meaningOfLive = ZIO.succeed(42)
  val favLang       = ZIO.succeed("Scala")

  // Fiber = lightweight thread
  // A description of a computation which will be performed by one of the threads
  // managed by the ZIO runtime

  def createFiber: Fiber[Throwable, String] = ??? // fibers are almost impossible to create manually

  val sameThreadIO =
    for
      mol  <- meaningOfLive.debugThread
      lang <- favLang.debugThread
    yield (mol, lang)

  val differentThreadIO =
    for
      _ <- meaningOfLive.debugThread.fork
      _ <- favLang.debugThread.fork
    yield ()

  val meaningOfLiveFiber: ZIO[Any, Nothing, Fiber[Throwable, Int]] = meaningOfLive.fork

  // join a fiber
  def runOnAnotherThread[R, E, A](zio: ZIO[R, E, A]) =
    for
      fib    <- zio.fork
      result <- fib.join
    yield result

  // awaiting a fiber
  def runOnAnotherThread_v2[R, E, A](zio: ZIO[R, E, A]) =
    for
      fib    <- zio.fork
      result <- fib.await
    yield result match
      case Exit.Success(value) => s"succeeded with $value"
      case Exit.Failure(cause) => s"failed with $cause"

  // poll - peek at the result of the fiber RIGHT NOW, without blocking
  val peekFiber =
    for
      fib <- ZIO.attempt {
        Thread.sleep(1000)
        42
      }.fork
      result <- fib.poll
    yield result

  // compose fibers
  // zip
  val zippedFibers =
    for
      fib1 <- ZIO.succeed("Result from fiber 1").debugThread.fork
      fib2 <- ZIO.succeed("Result from fiber 2").debugThread.fork
      fiber = fib1.zip(fib2)
      tuple <- fiber.join
    yield tuple

  // orElse
  val chainedFibers =
    for
      fiber1 <- ZIO.fail("not good").debugThread.fork
      fiber2 <- ZIO.succeed("rock the JVM!").debugThread.fork
      fiber = fiber1.orElse(fiber2)
      message <- fiber.join
    yield message

  // --------------------------------------------------------------------------

  // def run = sameThreadIO
  // [ZScheduler-Worker-5] 42
  // [ZScheduler-Worker-5] Scala

  // def run = differentThreadIO
  // [ZScheduler-Worker-5] 42
  // [ZScheduler-Worker-8] Scala
  // [ZScheduler-Worker-8][FAIL] Interrupt(Runtime(6,1666003328584,),Stack trace for thread "zio-fiber-":)
  // [ZScheduler-Worker-5][FAIL] Interrupt(Runtime(6,1666003328584,),Stack trace for thread "zio-fiber-":)

  // def run = runOnAnotherThread(meaningOfLive).debugThread
  // [ZScheduler-Worker-8] 42

  // def run = runOnAnotherThread_v2(meaningOfLive).debugThread
  // [ZScheduler-Worker-4] succeeded with 42

  // def run = peekFiber.debugThread
  // [ZScheduler-Worker-4] succeeded with 42

  // def run = zippedFibers.debugThread
  // [ZScheduler-Worker-0] Result from fiber 1
  // [ZScheduler-Worker-5] Result from fiber 2
  // [ZScheduler-Worker-9] (Result from fiber 1,Result from fiber 2)

  def run = chainedFibers.debugThread
  // [ZScheduler-Worker-1] rock the JVM!
  // [ZScheduler-Worker-10][FAIL] Fail(not good,Stack trace for thread "zio-fiber-7":
  //	at com.rockthejvm.part3concurrency.Fibers.chainedFibers(Fibers.scala:70)
  // 	at com.rockthejvm.utils.Utils$package.debugThread(Utils.scala:8)
  //	at com.rockthejvm.utils.Utils$package.debugThread(Utils.scala:11)
  //	at com.rockthejvm.part3concurrency.Fibers.chainedFibers(Fibers.scala:70))
  // [ZScheduler-Worker-11] rock the JVM!
