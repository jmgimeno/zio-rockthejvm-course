package com.rockthejvm.part3concurrency

import zio.*
import com.rockthejvm.utils.debugThread

object MasteringInterruptions extends ZIOAppDefault:

  // interruptions:
  // - fib.interrupt
  // - ZIO.race, ZIO.zipPar, ZIO.collectAllPar
  // - outliving parent fiber

  // manual interruption
  val aManualInterruptedZIO =
    ZIO.succeed("computing...").debugThread *> ZIO.interrupt *> ZIO.succeed(42).debugThread

  // finalizer
  val effectWithInterruptionFinalizer =
    aManualInterruptedZIO.onInterrupt(ZIO.succeed("I was interrupted").debugThread)

  // uninterruptable
  // payment flow to NOT be interrupted
  val fussyPaymentSystem = (
    ZIO.succeed("payment running, don't cancel me...").debugThread *>
      ZIO.sleep(1.second) *> // the actual payment
      ZIO.succeed("payment completed").debugThread
  ).onInterrupt(ZIO.succeed("MEGA CANCEL OF DOOM").debugThread) // don't want this triggered

  val cancellationOfDoom = for
    fib <- fussyPaymentSystem.fork
    _   <- ZIO.sleep(500.millis) *> fib.interrupt
    _   <- fib.join
  yield ()

  val atomicPayment    = ZIO.uninterruptible(fussyPaymentSystem) // make a ZIO atomic
  val atomicPayment_v2 = fussyPaymentSystem.uninterruptible      // same

  val noCancellationOfDoom =
    for
      fib <- atomicPayment.fork
      _   <- ZIO.sleep(500.millis) *> fib.interrupt
      _   <- fib.join
    yield ()

  // interruptability is regional
  val zio1        = ZIO.succeed(1)
  val zio2        = ZIO.succeed(2)
  val zio3        = ZIO.succeed(3)
  val zioComposed = (zio1 *> zio2 *> zio3).uninterruptible // all the zios are uninterruptible
  val zioConposed2 =
    (zio1 *> zio2.interruptible *> zio3).uninterruptible // inner scopes override outer scopes

  // format: OFF
  
  // uninterruptibleMask
  /*
    example: an authentication service
    - input password, can be interrupted, because otherwise it might block the fiber indefinitely
    - verify password, which cannot be interrupted once it's triggered
  */
  // format: ON

  val inputPassword =
    for
      _    <- ZIO.succeed("Input password:").debugThread
      _    <- ZIO.succeed("(typing password)").debugThread
      _    <- ZIO.sleep(2.seconds)
      pass <- ZIO.succeed("RockTheJVM1!")
    yield pass

  def verifyPassword(pw: String) =
    for
      _      <- ZIO.succeed("verifying...").debugThread
      _      <- ZIO.sleep(2.seconds)
      result <- ZIO.succeed(pw == "RockTheJVM1!")
    yield result

  val authFlow = ZIO.uninterruptibleMask { restore =>
    // EVERYTHING IS uninterruptible...
    for
      pw <- restore(inputPassword) /* <- ... except this thing*/.onInterrupt(
        /* ^^restores the interruptibility flag of this ZIO at the time of the call */
        ZIO.succeed("Authentication timed out. Try out again later.").debugThread
      )
      verification <- verifyPassword(pw)
      _ <-
        if verification
        then ZIO.succeed("Authentication successful.").debugThread
        else ZIO.succeed("Authentication failed.").debugThread
    yield ()
  }

  val authProgram =
    for
      authFib <- authFlow.fork
      _ <- ZIO.sleep(3.seconds) *> ZIO
        .succeed("Attempting to cancel authentication...")
        .debugThread *> authFib.interrupt
      _ <- authFib.join
    yield ()

  /*
   *  Exercises
   */

  // 1. what will these effects do? (without running them in your app)
  val cancelBeforeMol   = ZIO.interrupt *> ZIO.succeed(42).debugThread
  val uncancelBeforeMol = ZIO.uninterruptible(ZIO.interrupt *> ZIO.succeed(42).debugThread)
  // neither effect will print anything

  // 2
  val authProgram_v2 =
    for
      authFib <- ZIO.uninterruptibleMask(_ => authFlow).fork
      _ <- ZIO.sleep(3.seconds) *> ZIO
        .succeed("Attempting to cancel authentication...")
        .debugThread *> authFib.interrupt
      _ <- authFib.join
    yield ()
  // what happens if we type passwrod before/after the interruption?
  // - before/after: the same because now all authFlow cannot be interrupted

  /*
    Uninterruptible calls are masks which suppress cancellation. Restorer opens "gaps"
    in the uninterruptible region. If you wrap an entire structure with another
    .uninterruptible/.uninterruptibleMask, you'll cover those gaps too.
    For this example, the program will cover all interruptible gaps, so the
    interruption signals will be ignored
   */

  // 3
  val threeStepProgram =
    val sequence = ZIO.uninterruptibleMask { restore =>
      for
        _ <- restore(ZIO.succeed("interruptible").debugThread) *> ZIO.sleep(1.second)
        _ <- ZIO.succeed("uninterruptible").debugThread *> ZIO.sleep(1.second)
        _ <- restore(ZIO.succeed("interruptible").debugThread) *> ZIO.sleep(1.second)
      yield ()
    }
    for
      fib <- sequence.fork
      _   <- ZIO.sleep(1500.millis) *> ZIO.succeed("INTERRUPTING!!!").debugThread *> fib.interrupt
      _   <- fib.join
    yield ()
  // the interruptible signal arrives in the uninterruptible zone but, in the next restore, it interrupts
  // the restored region.

  val run = threeStepProgram
