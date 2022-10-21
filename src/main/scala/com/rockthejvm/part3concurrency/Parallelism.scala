package com.rockthejvm.part3concurrency

import zio.*
import zio.Exit.Success
import zio.Exit.Failure

import com.rockthejvm.utils.debugThread

object Parallelism extends ZIOAppDefault:

  val meaningOfLive = ZIO.succeed(42)
  val favLang       = ZIO.succeed("Scala")

  // combines sequentially
  val combined = meaningOfLive.zip(favLang)

  // combines in parallel
  val combinedPar = meaningOfLive.zipPar(favLang)

  /*
    - starts each on a fibers
    - what if one fails? the other must be interrupted
    - what if one is interrupted? the entire thing must be interrupted
    - what if the whole thing is interrupted? need to interrupt both effects
   */

  // try a zipPar combinator
  // hint: fork/join/await, interrupt
  // NOTE: it's not 100% accurate !!!!
  def myZipPar[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, (A, B)] =
    val exits = for
      fiba  <- zioa.fork
      fibb  <- ziob.fork
      exita <- fiba.await // <- we depend on value of fiba
      exitb <- exita match
        case Success(value) => fibb.await
        case Failure(cause) => fibb.interrupt
    yield (exita, exitb)

    exits.flatMap {
      case (Success(a), Success(b))           => ZIO.succeed((a, b))
      case (Success(_), Failure(cause))       => ZIO.failCause(cause)
      case (Failure(cause), Success(_))       => ZIO.failCause(cause)
      case (Failure(causea), Failure(causeb)) => ZIO.failCause(Cause.Both(causea, causeb))
    }

  // parallel combinators
  // zipPar, zipWithPar

  // collectAllPar/collectParDiscard
  val effects: IndexedSeq[ZIO[Any, Nothing, Int]] = (1 to 10).map(i => ZIO.succeed(i).debugThread)
  val collectedvalues: ZIO[Any, Nothing, IndexedSeq[Int]] = ZIO.collectAllPar(effects) // traverse

  // foreachPar/foreachParDiscard
  val printlnParallel = ZIO.foreachParDiscard(1 to 10)(i => ZIO.succeed(println(i)))

  // reduceAllPar, mergeAllPar
  val sumPar    = ZIO.reduceAllPar(ZIO.succeed(0), effects)(_ + _)
  val sumPar_v2 = ZIO.mergeAllPar(effects)(0)(_ + _)

  /*
    - if all the effects succeed. all good
    - one effect fails => everyone else is interrupted, error is surfaced
    - one effect is interrupted => everyone else is interrupted, error = interruption (for the big expression)
    - if the entire thing is interrupted => all effects are interrupted as well
   */

  /*
    Exercise: do the word counting but using a parallel combinator
   */

  def countWords(path: String): UIO[Int] =
    ZIO.succeed {
      val source = scala.io.Source.fromFile(path)
      val nWords = source.getLines().mkString(" ").split("\\s+").size
      source.close()
      nWords
    }.debugThread

  def simulateCountWords(path: String): UIO[Int] =
    ZIO.succeed {
      println(s"Opening $path")
      val lapse = scala.util.Random.between(500, 1000)
      Thread.sleep(lapse)
      println(s"Closing $path")
      lapse
    }.debugThread

  def countWordParallel_v1(n: Int): UIO[Int] =
    ZIO.reduceAllPar(
      ZIO.succeed(0),
      (1 to n).map(i => countWords(s"src/main/resources/testfile_$i.txt"))
    )(_ + _)

  def countWordParallel_v2(n: Int): UIO[Int] =
    ZIO.mergeAllPar(
      (1 to n).map(i => countWords(s"src/main/resources/testfile_$i.txt"))
    )(0)(_ + _)

  def countWordParallel_v3(n: Int): UIO[Int] =
    ZIO
      .foreachPar(1 to n)(i => countWords(s"src/main/resources/testfile_$i.txt"))
      .map(_.sum)

  def countWordParallel_v4(n: Int): UIO[Int] =
    ZIO
      .collectAllPar((1 to n).map(i => countWords(s"src/main/resources/testfile_$i.txt")))
      .map(_.sum)

  def run = countWordParallel_v4(10).debugThread
