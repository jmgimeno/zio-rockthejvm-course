package com.rockthejvm.part3concurrency

import zio.*
import com.rockthejvm.utils.*
import java.io.FileWriter
import java.io.File
import java.io.IOException
import java.io.FileReader

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

  // def run = chainedFibers.debugThread
  // [ZScheduler-Worker-1] rock the JVM!
  // [ZScheduler-Worker-10][FAIL] Fail(not good,Stack trace for thread "zio-fiber-7":
  //	at com.rockthejvm.part3concurrency.Fibers.chainedFibers(Fibers.scala:70)
  // 	at com.rockthejvm.utils.Utils$package.debugThread(Utils.scala:8)
  //	at com.rockthejvm.utils.Utils$package.debugThread(Utils.scala:11)
  //	at com.rockthejvm.part3concurrency.Fibers.chainedFibers(Fibers.scala:70))
  // [ZScheduler-Worker-11] rock the JVM!

  /*
   * Exercises
   */

  // 1 - zip two fibers without using the zip combinator
  // hint: create a fiber that waits for both of these fibers
  def zipFibers[E, A, B](
      fiber1: Fiber[E, A],
      fiber2: Fiber[E, B]
  ): ZIO[Any, Nothing, Fiber[E, (A, B)]] =
    val finalEffect =
      for
        result1 <- fiber1.join
        result2 <- fiber2.join
      yield (result1, result2)
    finalEffect.fork

  // this is not the best solution but, with our knowledge, it's good enough right now

  val zippedFibers_v2 =
    for
      fib1   <- ZIO.succeed("Result from fiber 1").debugThread.fork
      fib2   <- ZIO.succeed("Result from fiber 2").debugThread.fork
      fiber  <- zipFibers(fib1, fib2)
      result <- fiber.join
    yield result

  // Let's allow for a different errors in the fibers
  def zipFibersGeneral[E, E1 <: E, E2 <: E, A, B](
      fiber1: Fiber[E1, A],
      fiber2: Fiber[E2, B]
  ): ZIO[Any, Nothing, Fiber[E, (A, B)]] =
    val finalEffect =
      for
        result1 <- fiber1.join
        result2 <- fiber2.join
      yield (result1, result2)
    finalEffect.fork

  // 2 - same thing with orElse
  def chainedFibers[E, A](
      fiber1: Fiber[E, A],
      fiber2: Fiber[E, A]
  ): ZIO[Any, Nothing, Fiber[E, A]] =
    fiber1.join.orElse(fiber2.join).fork

  // 3 - distributing a task between many fibers
  // spawn n fibers, count the number of words in each file,
  // then aggregate all the results together in one big number
  def generateRandomFile(path: String): Unit = {
    val random = scala.util.Random
    val chars  = 'a' to 'z'
    val nWords = random.nextInt(2000)
    val content =
      (1 to nWords)
        .map(_ => (1 to random.nextInt(10)).map(_ => chars(random.nextInt(26))).mkString)
        .mkString(" ")
    val writer = new FileWriter(new File(path))
    writer.write(content)
    writer.flush()
    writer.close()
  }

  def countFile(path: String): ZIO[Any, Throwable, Int] = ZIO.attempt {
    // problems is file has more than one space between words !!!
    val reader = new FileReader(new File(path))
    var spaces = 0
    var ch     = reader.read()
    while (ch != -1) {
      if (ch == ' ') spaces += 1
      ch = reader.read()
    }
    reader.close()
    spaces + 1
  }

  def spawnCounters(n: Int): ZIO[Any, Throwable, List[Int]] =
    if n == 0
    then ZIO.succeed(List())
    else
      for
        fib1 <- countFile(s"src/main/resources/testfile_$n.txt").debugThread.fork
        fib2 <- spawnCounters(n - 1).fork
        head <- fib1.join
        tail <- fib2.join
      yield head :: tail

  def countWords(n: Int): ZIO[Any, Throwable, Int] =
    spawnCounters(n).map(_.sum)

  // The official solution does not count the files in parallel.
  // use simulateCountWords !!!

  // part 1 - an effect which reads one file and counts the words there
  def countWords(path: String): UIO[Int] =
    ZIO.succeed {
      val source = scala.io.Source.fromFile(path)
      val nWords = source.getLines().mkString(" ").split("\\s+").size
      nWords
    }.debugThread

  // part 2 - spin up fibers for all the files
  def wordCountParallel(n: Int): UIO[Int] =
    val effects = (1 to n)
      .map(i => s"src/main/resources/testfile_$i.txt") // list of paths
      .map(countWords)                                 // list of effects
      .map(_.fork)                                     // list of effects returning fibers
      .map(fiberEff => fiberEff.flatMap(_.join))       // list of effects returning integers

    effects.reduce { (zioa, ziob) =>
      for
        a <- zioa
        b <- ziob
      yield a + b
    }

  def simulateCountWords(path: String): UIO[Int] =
    ZIO.succeed {
      println(s"Opening $path")
      val lapse = scala.util.Random.between(500, 1000)
      Thread.sleep(lapse)
      println(s"Closing $path")
      lapse
    }.debugThread

  def wordCountParallel_v2(n: Int): UIO[Int] =
    val effects = (1 to n)
      .map { i =>
        for
          fiberCountFile <- countWords(s"src/main/resources/testfile_$i.txt").fork
          countFile      <- fiberCountFile.join
        yield countFile
      }

    effects.reduce { (zioa, ziob) =>
      for
        a <- zioa
        b <- ziob
      yield a + b
    }

  // def run =
  //   ZIO.succeed((1 to 10).foreach(i => generateRandomFile(s"src/main/resources/testfile_$i.txt")))

  // def run = zippedFibers_v2.debugThread

  // def run = countWords(10).debugThread
  // [ZScheduler-Worker-3] 843
  // [ZScheduler-Worker-9] 740
  // [ZScheduler-Worker-10] 1180
  // [ZScheduler-Worker-1] 923
  // [ZScheduler-Worker-5] 986
  // [ZScheduler-Worker-0] 1370
  // [ZScheduler-Worker-4] 1429
  // [ZScheduler-Worker-6] 1586
  // [ZScheduler-Worker-11] 1766
  // [ZScheduler-Worker-7] 1712
  // [ZScheduler-Worker-6] 12535

  def run = wordCountParallel_v2(10).debugThread
  // [ZScheduler-Worker-7] 876
  // [ZScheduler-Worker-3] 1426
  // [ZScheduler-Worker-10] 674
  // [ZScheduler-Worker-1] 1541
  // [ZScheduler-Worker-0] 1266
  // [ZScheduler-Worker-5] 1598
  // [ZScheduler-Worker-6] 768
  // [ZScheduler-Worker-4] 1212
  // [ZScheduler-Worker-7] 828
  // [ZScheduler-Worker-3] 1062
  // [ZScheduler-Worker-3] 11251

  // jmgimeno:resources/ (development*) $ wc -w *                                                [16:39:28]
  //      876 testfile_1.txt
  //     1062 testfile_10.txt
  //     1426 testfile_2.txt
  //      674 testfile_3.txt
  //     1541 testfile_4.txt
  //     1266 testfile_5.txt
  //     1598 testfile_6.txt
  //      768 testfile_7.txt
  //     1212 testfile_8.txt
  //      828 testfile_9.txt
  //    11251 total
