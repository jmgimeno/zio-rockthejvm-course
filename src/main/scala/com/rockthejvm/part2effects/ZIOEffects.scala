package com.rockthejvm.part2effects

import com.rockthejvm.part2effects.ZIOEffects.runForever
import zio.*

import scala.io.StdIn

object ZIOEffects {

  // success
  val meaningOfLife: ZIO[Any, Nothing, Int] = ZIO.succeed(42)
  // failre
  val failure: ZIO[Any, String, Nothing] = ZIO.fail("Something went wrong")
  // suspension/delay
  val suspendedZIO: ZIO[Any, Throwable, Int] = ZIO.suspend(meaningOfLife)

  // map + flatMap
  val improvedMOL = meaningOfLife.map(_ * 2)
  val printingMOL = meaningOfLife.flatMap(mol => ZIO.succeed(println(mol)))
  // for comprehensions
  val smallProgram = for {
    _ <- ZIO.succeed(println("what's your name?"))
    name <- ZIO.succeed(StdIn.readLine())
    _ <- ZIO.succeed(println(s"Welcome to ZIO, $name"))
  } yield ()

  // ZIO has a LOT of combinators & transformers
  // zip, zipWith
  val anotherMOL = ZIO.succeed(100)
  val tupledZIO = meaningOfLife.zip(anotherMOL)
  val combinedZIO = meaningOfLife.zipWith(anotherMOL)(_ * _)

  /**
   * Type aliases of ZIOs
   */

  // UIO[A] = ZIO[Any, Nothing, A] - no requirements, cannot fail, produces A
  val aUIO: UIO[Int] = ZIO.succeed(99)

  // Task[A] = ZIO[Any, Throwable, A] - no requirements, can fail with a Throwable, produces A
  val aSuccessfulTask: Task[Int] = ZIO.succeed(89)
  val aFailedTask: Task[Int] = ZIO.fail(new RuntimeException("Something bad"))

  // IO[E, A] = ZIO[Any, E, A] - no requirements, can fail with an E, produces A
  val aSuccessfulIO: IO[String, Int] = ZIO.succeed(34)
  val aFailedIO: IO[String, Int] = ZIO.fail("Something bad happened")

  // Less used:

  // URIO[R, A] = ZIO[R, Nothing, A] - cannot fail but have some requirements
  val aURIO: URIO[Int, Int] = ZIO.succeed(67)
  // RIO[R, A] = ZIO[R, Throwable, A] - can fail with a Throwable
  val anRIO: RIO[Int, Int] = ZIO.succeed(89)
  val aFailedRIO: RIO[Int, Int] = ZIO.fail(new RuntimeException("RIO failed"))

  /**
   * Exercises
   */

  // 1 - sequence two ZIOs and take the value of the last one
  def sequenceTakeLast[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, B] =
    zioa.flatMap { _ => ziob }

  def sequenceTakeLast_v2[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, B] =
    for {
      _ <- zioa
      b <- ziob
    } yield b

  def sequenceTakeLast_v3[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, B] =
    zioa *> ziob

  // 2 - sequence two ZIOa abd take the value of the first one
  def sequenceTakeFirst[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, A] =
    zioa.flatMap { a =>
      ziob.map { _ =>
        a
      }
    }

  def sequenceTakeFirst_v2[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, A] =
    zioa <* ziob

  // 3 - run a  ZIO forever
  def runForever[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
    zio.flatMap { _ => runForever(zio) }

  def runForever_v2[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
    zio *> runForever_v2(zio)

  val endlessLoop = runForever {
    ZIO.succeed {
      println("running...")
      Thread.sleep(1000)
    }
  }

  // 4 - convert the value of a ZIO to something else
  def convert[R, E, A, B](zio: ZIO[R, E, A], value: B): ZIO[R, E, B] =
    zio.map { _ => value }

  def convert_v2[R, E, A, B](zio: ZIO[R, E, A], value: B): ZIO[R, E, B] =
    zio.as(value)

  // 5 - discard the valuef a ZIO to unit
  def asUnit[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, Unit] =
    convert(zio, ())

  def asUnit_v2[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, Unit] =
    zio.unit

  // 6 - recursion
  def sum(n: Int): Int =
    if n == 0 then 0 else n + sum(n - 1) // will crash with stack overflow

  def sumZIO(n: Int): UIO[Int] =
    if n == 0 then ZIO.succeed(0)
    else ZIO.succeed(n).flatMap { n =>
      sumZIO(n - 1).map { s =>
        n + s
      }
    }

  def sumZIO_v2(n: Int): UIO[Int] =
    if n == 0 then ZIO.succeed(0)
    else for {
      current <- ZIO.succeed(n)
      prevSum <- sumZIO(n - 1)
    } yield current + prevSum

  // 7 - fibonacci
  // hint: use ZIO.suspend/ZIO.suspendsucceed
  def fibo(n: Int): BigInt =
    if n <= 2 then 1 else fibo(n - 1) + fibo(n - 2)

  def fiboZIO_bad(n: Int): UIO[BigInt] =
    if n <= 2 then ZIO.succeed(n)
    else for {
      last <- fiboZIO_bad(n - 1)
      prev <- fiboZIO_bad(n - 2)
    } yield last + prev // this stack overflows

  /*
    fiboZIO(n -1).flatMap(last => fiboZIO(n - 2).map(prev => last + prev))
    ^^^^^^^^^^^^^
        is evaluated before the flatMap !!!
  */

  def fiboZIO(n: Int): UIO[BigInt] =
    if n <= 2 then ZIO.succeed(n)
    else for {
      last <- ZIO.suspendSucceed(fiboZIO_bad(n - 1))
      prev <- fiboZIO_bad(n - 2)
    } yield last + prev

  def main(args: Array[String]): Unit = {
    val runtime = Runtime.default

    given trace: Trace = Trace.empty

    Unsafe.unsafe { u ?=>
      val sumZ = sumZIO(20000)
      val fibZ = fiboZIO(20)
      val sum = runtime.unsafe.run(fibZ)
      println(sum)
    }
  }
}
