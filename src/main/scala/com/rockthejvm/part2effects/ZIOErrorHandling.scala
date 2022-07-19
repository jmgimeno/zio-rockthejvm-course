package com.rockthejvm.part2effects

import java.io.IOException
import scala.util.{Try, Success, Failure}

import zio.*
import java.net.NoRouteToHostException

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
  val catchError =
    anAttempt.catchAll(e => ZIO.succeed(s"Returning a different value $e"))

  // catchSome maintains the error channel and even can widen it
  val catchSelectedErrors = anAttempt.catchSome {
    case e: RuntimeException => ZIO.succeed(s"Ignoring runtime exceptions: $e")
    case _                   => ZIO.succeed("Ignoring everything else")
  }

  // chain effects
  val aBetterAttempt = anAttempt.orElse(ZIO.succeed(56))

  // fold: handle both success and failure
  val handleBoth = anAttempt.fold(
    ex => s"Something bad $ex",
    value => s"Length of string was $value"
  )
  // effectful fold
  val handleBoth_v2 = anAttempt.foldZIO(
    ex => ZIO.succeed(s"Something bad $ex"),
    value => ZIO.succeed(s"Length of string was $value")
  )

  /*
    Conversions between Option/Try/Either to ZIO
   */

  // Try -> ZIO
  val aTryToZIO: ZIO[Any, Throwable, Int] =
    ZIO.fromTry(Try(42 / 0)) // can fail with throwable

  // Either -> ZIO
  val anEither: Either[Int, String] = Right("Success!")
  val anEitherToZIO: ZIO[Any, Int, String] = ZIO.fromEither(anEither)

  // ZIO -> ZIO with either as the value channel
  val eitherZIO: ZIO[Any, Nothing, Either[Throwable, Int]] = anAttempt.either
  // reverse
  val anAttempt_v2 = eitherZIO.absolve

  // option -> ZIO
  val anOption: ZIO[Any, Option[Nothing], Int] = ZIO.fromOption(Some(42))

  /** Exercise: implement versions of fromTry, fromOption, fromEither, either,
    * absolve using fold and foldZIO
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

  /*
    Errors = failures present in the ZIO type signature ("checked" errors)
    Defects = failures that are unrecoverable, unforeseen, NOT present in the type signature

    ZIO[R, E, A] can finish with Exit[E, A]
      - Success[A] containing value A
      - Cause[E]
        - Fail[E] containing the error
        - Die(t: Throwable) which was unforeseen
   */
  val divisionByZero: UIO[Int] = ZIO.succeed(1 / 0)

  val failedInt: ZIO[Any, String, Int] = ZIO.fail("I failed")
  val failureCauseExposed: ZIO[Any, Cause[String], Int] = failedInt.sandbox
  val failureCauseHidden: ZIO[Any, String, Int] = failureCauseExposed.unsandbox
  // fold with cause
  val foldedWithCause = failedInt.foldCause(
    cause => s"this failed with ${cause.defects}",
    value => s"this succeeded with $value"
  )
  val foldedWithCause_v2 = failedInt.foldCauseZIO(
    cause => ZIO.succeed(s"this failed with ${cause.defects}"),
    value => ZIO.succeed("this succeeded with $value")
  )

  /*
    Good practice:
      - at a lower level, you "errors" should be treated
      - at a higher level, you should hide "errors" and assume they are unrecoverable
   */
  def callHTTPEndpoint(url: String): ZIO[Any, IOException, String] =
    ZIO.fail(new IOException("no internet, dummy!"))

  val endpointCallWithDefects: ZIO[Any, Nothing, String] =
    callHTTPEndpoint("rockthejvm.com").orDie // all errors are now defects

  // refining the error channel
  def callHTTPEndpointWideError(url: String): ZIO[Any, Exception, String] =
    ZIO.fail(new IOException("no internet, dummy!"))

  def callHTTPEndpoint_v2(url: String): ZIO[Any, IOException, String] =
    callHTTPEndpointWideError(url).refineOrDie[IOException] {
      case e: IOException => e
      case _: NoRouteToHostException =>
        new IOException(s"No route to host to $url, can't fetch page")
    }

  // reverse: turn defects into the error channel
  val endpointCallWithError = endpointCallWithDefects.unrefine {
    case e: Throwable => e.getMessage
  }

  /*
    Combine effects with different errors
   */
  case class IndexError(message: String)
  case class DbError(message: String)

  val callApi: ZIO[Any, IndexError, String] = ZIO.succeed("page: <html></html>")
  val queryDB: ZIO[Any, DbError, Int] = ZIO.succeed(1)
  val combined: ZIO[Any, IndexError | DbError, (String, Int)] = for {
    page <- callApi
    rowsAffected <- queryDB
  } yield (page, rowsAffected) // lost type safety (error channel is Object)

  /*
    Solution:
      - design an error model (make the error extend AppError)
      - use Scala 3 union types (as shown)
      - .mapError to some common error type
   */

  /** 
   * Exercises
   */

  // 1 - make this effect fail with a TYPED error
  val aBadFailure = ZIO.succeed(throw new RuntimeException("this is bad!"))
  val aBetterFailure = aBadFailure.unrefine { // surfaces out the exception in the error channel
    case e => e
  }

  val aBetterFailure_v2 = aBadFailure.sandbox // exposes the defect in the cause

  // 2 - transform a zio into another zio with narrower exception type
  def ioException[R, A](zio: ZIO[R, Throwable, A]): ZIO[R, IOException, A] = 
    zio.refineOrDie {
      case e: IOException => e
    }

  // 3
  def left[R, E, A, B](zio: ZIO[R, E, Either[A, B]]): ZIO[R, Either[E, A], B] =
    zio.foldZIO(
      e => ZIO.fail(Left(e)),
      value => value.fold(a => ZIO.fail(Right(a)), b => ZIO.succeed(b))
    )

  // 4
  val database = Map(
    "daniel" -> 123,
    "alice" -> 789
  )

  case class QueryError(reason: String)
  case class UserProfile(name: String, phone: Int)

  def lookupProfile(userId: String): ZIO[Any, QueryError, Option[UserProfile]] =
    if userId != userId.toLowerCase
    then ZIO.fail(QueryError("user ID format is invalid"))
    else ZIO.succeed(database.get(userId).map(UserProfile(userId, _)))

  // surface out all the failed cases of this API
  def betterProfile(userId: String): ZIO[Any, Option[QueryError], UserProfile] = 
    lookupProfile(userId).foldZIO(
      e => ZIO.fail(Some(e)),
      profileOption => profileOption.fold(ZIO.fail(None))(p => ZIO.succeed(p))
    )

  def betterProfile_v2(userId: String): ZIO[Any, Option[QueryError], UserProfile] = 
    lookupProfile(userId).some
    
  override def run = ???
}
