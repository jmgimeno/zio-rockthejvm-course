package com.rockthejvm.part3concurrency

import zio.*

import com.rockthejvm.utils.debugThread

object Interruptions extends ZIOAppDefault:

  val zioWithTime = {
    ZIO.succeed("starting computation").debugThread *>
      ZIO.sleep(2.seconds) *>
      ZIO.succeed(42).debugThread
  }.onInterrupt(ZIO.succeed("I was interrupted").debugThread)
  // other method os onDone

  /*
    Manual interruption
   */

  val interruption = for
    fib    <- zioWithTime.fork
    _      <- ZIO.sleep(1.second) *> ZIO.succeed("interrupting").debugThread *> fib.interrupt
    _      <- ZIO.succeed("interruption successful").debugThread
    result <- fib.join
  yield result

  // fib.interrupt is an effect that will semantically block until the fiber has been successfully interrupted

  val interruption_v2 = for
    fib    <- zioWithTime.fork
    _      <- (ZIO.sleep(1.second) *> ZIO.succeed("interrupting").debugThread *> fib.interrupt).fork
    _      <- ZIO.succeed("interruption successful").debugThread
    result <- fib.join
  yield result

  // we leak a fiber (by not joining it), but it will be cleaned by the GC

  val interruption_v2_alt = for
    fib    <- zioWithTime.fork
    _      <- ZIO.sleep(1.second) *> ZIO.succeed("interrupting").debugThread *> fib.interruptFork
    _      <- ZIO.succeed("interruption successful").debugThread
    result <- fib.join
  yield result

  // fib.interruptFork, interrupts from a new fiber

  /*
    Automatic interruption
   */

  // outliving a parent fiber
  val parentEffect =
    ZIO.succeed("spawning fiber").debugThread *>
      // zioWithTime.fork *> // child fiber
      zioWithTime.forkDaemon *> // this fiber will be a child of the MAIN fiber
      ZIO.sleep(1.second) *>
      ZIO.succeed("parent successful").debugThread

  val testOutlivingParent = for
    parentEffectFib <- parentEffect.fork
    _               <- ZIO.sleep(3.seconds)
    _               <- parentEffectFib.join
  yield ()
  // child fibers will be (automatically) interrupted if the parent fiber is completed

  // with .fork
  // [ZScheduler-Worker-2] spawning fiber
  // [ZScheduler-Worker-9] starting computation
  // [ZScheduler-Worker-11] parent successful
  // [ZScheduler-Worker-1] I was interrupted

  // with forkDaemon
  // [ZScheduler-Worker-2] spawning fiber
  // [ZScheduler-Worker-9] starting computation
  // [ZScheduler-Worker-1] parent successful
  // [ZScheduler-Worker-4] 42

  // racing
  val slowEffect = {
    ZIO.succeed("starting slow").debugThread *>
      ZIO.sleep(2.seconds) *>
      ZIO.succeed("slow").debugThread
  }.onInterrupt(ZIO.succeed("[slow] interrupted").debugThread)

  val fastEffect = {
    ZIO.succeed("starting fast").debugThread *>
      ZIO.sleep(1.second) *>
      ZIO.succeed("fast").debugThread
  }.onInterrupt(ZIO.succeed("[fast] interrupted").debugThread)

  val aRace = slowEffect.race(fastEffect)

  val testRace = aRace.fork *> ZIO.sleep(3.seconds)
  // the losing fiber is automatically interrupted when the race finishes

  /*
    Exercises
   */

  /*
    1 - implement a timeout function
      - if zio is successful before timeout => a successful effect
      - if zio fails before timeout => a failed effect
      - if zio takes longer => interrupt the effect
   */
  def timeout[R, E, A](zio: ZIO[R, E, A], time: Duration): ZIO[R, E, A] =
    for
      fib    <- zio.fork
      _      <- (ZIO.sleep(time) *> fib.interrupt).fork
      result <- fib.join
    yield result

  /*
    2 - timeout v2
      - if zio is successful before timeout => a successful effect with Some(a)
      - if zio fails before timeout => a failed effect
      - if zio takes longer => interrupt the effect, return a successful effect with None
      hint: foldCauseZIO
   */
  def timeout_v2[R, E, A](zio: ZIO[R, E, A], time: Duration): ZIO[R, E, Option[A]] =
    timeout(zio, time)
      .foldCauseZIO(
        {
          case Cause.Interrupt(_, _) => ZIO.succeed(None)
          case Cause.Fail(e, _)      => ZIO.fail(e)
          case _                     => ZIO.dieMessage("unknown")
        },
        { a =>
          ZIO.succeed(Some(a))
        }
      )

  def run = testRace
