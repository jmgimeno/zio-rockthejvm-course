package com.rockthejvm.part3concurrency

import zio.*
import com.rockthejvm.utils.debugThread
import java.util.Scanner
import java.io.File

object Resources extends ZIOAppDefault:

  // finalizers
  def unsafeMethod(): Int = throw new RuntimeException("not an int here for you")
  val anAttempt           = ZIO.attempt(unsafeMethod())

  // finalizers are run no matter what
  val attemptWithFinalizer = anAttempt.ensuring(ZIO.succeed("finalizer").debugThread)
  // multiple finalizer
  val attemptWith2Finalizers =
    attemptWithFinalizer.ensuring(ZIO.succeed("another finalizer").debugThread)
  // finalizers are run before the error is surfaces us in the console
  // .onInterrupt, .onError, .onDone, .onExit

  // resource lifecycle
  class Connection(url: String):
    val open  = ZIO.succeed(s"opening connection to $url...").debugThread
    val close = ZIO.succeed(s"closing connection to $url").debugThread

  object Connection:
    def create(url: String) = ZIO.succeed(new Connection(url))

  val fetchUrl =
    for
      conn <- Connection.create("rockthejvm.com")
      fib  <- (conn.open *> ZIO.sleep(300.seconds)).fork
      _    <- ZIO.sleep(1.second) *> ZIO.succeed("interrupting").debugThread *> fib.interrupt
      _    <- fib.join
    yield () // resource leak

  val correctFetchUrl =
    for
      conn <- Connection.create("rockthejvm.com")
      fib  <- (conn.open *> ZIO.sleep(300.seconds)).ensuring(conn.close).fork
      _    <- ZIO.sleep(1.second) *> ZIO.succeed("interrupting").debugThread *> fib.interrupt
      _    <- fib.join
    yield () // preventing leaks

  // tedious

  /*
    acquireRelease
      - acquiring cannot be interrupted
      - all finalizers are guaranteed to run
   */

  val cleanConnection = ZIO.acquireRelease(Connection.create("rockthejv,.com"))(_.close)

  // we only have to take care of business logic, not with lifecycle
  // Scope is provided by ZIOAppDefeult in main
  val fetchWithResource: ZIO[Scope, Nothing, Unit] =
    for
      conn <- cleanConnection
      fib  <- (conn.open *> ZIO.sleep(300.seconds)).fork
      _    <- ZIO.sleep(1.second) *> ZIO.succeed("interrupting").debugThread *> fib.interrupt
      _    <- fib.join
    yield ()

  // ZIO.scoped eliminates the Scope (it makes the resources lifespan finite)
  val fetchWithScopeResource: ZIO[Any, Nothing, Unit] = ZIO.scoped(fetchWithResource)

  // acquireReleaseWith
  val cleanConnection_v2: ZIO[Any, Nothing, Unit] = ZIO.acquireReleaseWith(
    Connection.create("rockthejvm.com") // acquire
  )(
    _.close                                     // release
  )(conn => conn.open *> ZIO.sleep(300.seconds) // use
  )

  val fetchWithResource_v2 =
    for
      fib <- cleanConnection_v2.fork
      _   <- ZIO.sleep(1.second) *> ZIO.succeed("interrupting").debugThread *> fib.interrupt
    yield ()

  /** Exercises
    *   1. use the acquireRelease to open a file, print all lines, (one every 100 millis), then
    *      close the file
    */
  def openFileScanner(path: String): UIO[Scanner] =
    ZIO.succeed(new Scanner(new File(path)))

  def readLines(scanner: Scanner): UIO[Unit] =
    if scanner.hasNext()
    then Console.printLine(scanner.nextLine()).orDie *> ZIO.sleep(100.millis) *> readLines(scanner)
    else ZIO.unit

  def acquireOpenFile(path: String): UIO[Unit] =
    ZIO.acquireReleaseWith(
      openFileScanner(path)
    )(scanner => ZIO.succeed(scanner.close))(scanner => readLines(scanner))

  val testInterruptFileDisplay =
    for
      fib <- acquireOpenFile("src/main/scala/com/rockthejvm/part3concurrency/Resources.scala").fork
      _   <- ZIO.sleep(2.seconds) *> fib.interrupt
    yield ()

  // acquireRelease vs. acquireReleaseWith

  // nested resource acquisition difficult to read
  def connectionFromConfig(path: String): UIO[Unit] =
    ZIO.acquireReleaseWith(openFileScanner(path))(scanner =>
      ZIO.succeed("closing file").debugThread *> ZIO.succeed(scanner.close)
    )(scanner =>
      ZIO.acquireReleaseWith(Connection.create(scanner.nextLine()))(_.close) { conn =>
        conn.open *> ZIO.never
      }
    )

  // nested resource
  def connectionFromConfig_v2(path: String): UIO[Unit] = ZIO.scoped {
    for
      scanner <- ZIO.acquireRelease(openFileScanner(path))(scanner =>
        ZIO.succeed("closing file").debugThread *> ZIO.succeed(scanner.close)
      )
      conn <- ZIO.acquireRelease(Connection.create(scanner.nextLine()))(_.close)
      _    <- conn.open *> ZIO.never
    yield ()
  }

  // Why closing messages are not shown?
  //   It seems the problem lays in that vscode closes output to console too early
  //   https://github.com/scalameta/metals/issues/2043

  def run = connectionFromConfig("src/main/resources/connection.conf")
