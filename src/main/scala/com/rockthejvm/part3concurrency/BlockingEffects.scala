package com.rockthejvm.part3concurrency

import zio.*
import com.rockthejvm.utils.debugThread
import java.util.concurrent.atomic.AtomicBoolean

object BlockingEffects extends ZIOAppDefault:

  def blockingTask(n: Int): UIO[Unit] =
    ZIO.succeed(s"running blocking task $n").debugThread
      *> ZIO.succeed(Thread.sleep(10000))
      *> blockingTask(n)

  val program = ZIO.foreachPar(1 to 100)(blockingTask)
  // thread starvation

  // val run = program

  // blocking thread pool
  val aBlockingZIO = ZIO.attemptBlocking {
    println(s"[{Thread.currentThread().getName}] running a long computation...")
    Thread.sleep(10000)
    42
  }

  // val run = aBlockingZIO

  // blocking code cannot (usually) be interrupted
  val tryInterrupting =
    for
      blockingFib <- aBlockingZIO.fork
      _ <- ZIO
        .sleep(1.second) *> ZIO.succeed("interrupting...").debugThread *> blockingFib.interrupt
      mol <- blockingFib.join
    yield mol

  // val run = tryInterrupting

  // can use attemptBlockingInterrupt
  // based on Thread.interrupt -> InterruptedException
  val aBlockingInterruptibleZIO = ZIO.attemptBlockingInterrupt {
    println(s"[{Thread.currentThread().getName}] running a long computation...")
    Thread.sleep(10000)
    42
  }

  // set a flag/switch
  def interruptibleBlockingEffect(canceledFlag: AtomicBoolean): Task[Unit] =
    ZIO.attemptBlockingCancelable {
      (1 to 100000).foreach { element =>
        if !canceledFlag.get()
        then
          println(element)
          Thread.sleep(100)
      }
    }(ZIO.succeed(canceledFlag.set(true))) // canceling/interrupting effect

  val interruptibleBlockingDemo =
    for
      fib <- interruptibleBlockingEffect(new AtomicBoolean(false)).fork
      _   <- ZIO.sleep(2.seconds) *> ZIO.succeed("interrupting...").debugThread *> fib.interrupt
      _   <- fib.join
    yield ()

  // val run = interruptibleBlockingDemo

  // SEMANTIC blocking -no blocking threads, descheduling the effect/fiber
  val sleepingThread = ZIO.succeed(Thread.sleep(1000)) // blocking, uninterruptible
  val sleeping       = ZIO.sleep(1.second)             // SEMANTICALLY blocking, interruptible

  // yield
  // all effects evaluated on the same worker thread (BTW, I've needed to run it on sbt because on vscode it created problems)
  val chainedZIO = (1 to 1000).map(i => ZIO.succeed(i)).reduce(_.debugThread *> _.debugThread)

  // sometimes the runtime changes the worker thread
  val yieldingDemo =
    (1 to 1000).map(i => ZIO.succeed(i)).reduce(_.debugThread *> ZIO.yieldNow *> _.debugThread)

  val run = yieldingDemo
