package com.rockthejvm.part2effects

import scala.util.{Try, Success, Failure}

import zio.*

object ZIOErrorHandling extends ZIOAppDefault {

  // ZIOs can fail
  val aFailedZIO = ZIO.fail("Something went wrong")
  val failedWithThrowable = ZIO.fail(new RuntimeException("Boom!"))
  val failedWithMessage = failedWithThrowable.mapError(_.getMessage)

  // attempt: run an effect that may throw an exception
  val badZIO = ZIO.succeed {
    println("Trying something")
    val string: String = null
    string.length
  } // This is bad

  // use attempt if you're not sure whether your code might throw
  val anAttempt: ZIO[Any, Throwable, Int] = ZIO.attempt {
    println("Trying something")
    val string: String = null
    string.length
  }

  // effectfully catch errors
  val catchError = anAttempt.catchAll(e => ZIO.succeed(s"Returning a different value $e"))

  // catchSome maintains the error channel and even can widen it
  val catchSelectedErrors = anAttempt.catchSome {
    case e: RuntimeException => ZIO.succeed(s"Ignoring runtime exceptions: $e")
    case _ => ZIO.succeed("Ignoring everything else")
  }

  // chain effects
  val aBetterAttempt = anAttempt.orElse(ZIO.succeed(56))

  // fold: handle both success and failure
  val handleBoth = anAttempt.fold(ex => s"Something bad $ex", value => s"Length of string was $value")
  // effectful fold
  val handleBoth_v2 = anAttempt.foldZIO(
    ex => ZIO.succeed(s"Something bad $ex"),
    value => ZIO.succeed(s"Length of string was $value")
  )

  /*
    Conversions between Option/Try/Either to ZIO
  */

  // Try -> ZIO
  val aTryToZIO: ZIO[Any, Throwable, Int] = ZIO.fromTry(Try(42 / 0)) // can fail with throwable

  // Either -> ZIO
  val anEither: Either[Int, String] = Right("Success!")
  val anEitherToZIO: ZIO[Any, Int, String] = ZIO.fromEither(anEither)

  // ZIO -> ZIO with either as the value channel
  val eitherZIO: ZIO[Any, Nothing, Either[Throwable, Int]] = anAttempt.either
  // reverse
  val anAttempt_v2 = eitherZIO.absolve

  // option -> ZIO
  val anOption: ZIO[Any, Option[Nothing], Int] = ZIO.fromOption(Some(42))

  /**
   * Exercise: implement versions of fromTry, fromOption, fromEither, either, absolve
   * using fold and foldZIO
   */

  def try2ZIO[A](aTry: Try[A]): Task[A] =
    aTry.fold(ZIO.fail, ZIO.succeed)

  def either2ZIO[E, A](anEither: Either[E, A]): ZIO[Any, E, A] =
    anEither.fold(ZIO.fail, ZIO.succeed)

  def option2ZIO[A](anOption: Option[A]): ZIO[Any, Option[Nothing], A] =
    anOption.fold(ZIO.fail(None))(ZIO.succeed)

  def zio2ZioEither[R, E, A](zio: ZIO[R, E, A]): ZIO[R, Nothing, Either[E, A]] =
    zio.fold(Left.apply, Right.apply)

  def absolveZIO[R, E, A](zio: ZIO[R, Nothing, Either[E, A]]): ZIO[R, E, A] =
    zio.flatMap(_.fold(ZIO.fail, ZIO.succeed))

  override def run = ???
}
