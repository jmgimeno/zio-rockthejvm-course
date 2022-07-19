package com.rockthejvm.part2effects

import zio.*

object ZIOApps {

  val meaningOfLife : UIO[Int] = ZIO.succeed(42)

  def main(args: Array[String]): Unit = {
    val runtime = Runtime.default
    given trace: Trace = Trace.empty
    Unsafe.unsafeCompat { unsafe =>
      given u: Unsafe = unsafe

      println(runtime.unsafe.run(meaningOfLife))
    }
  }
}

object BetterApp extends ZIOAppDefault {
  // provides runtime, trace, ...

  def run = ZIOApps.meaningOfLife.debug
}

// Not needed the complexity in most cases
object ManualApp extends ZIOApp {

  override implicit def environmentTag = ???

  override type Environment = this.type

  override def bootstrap = ???

  override def run = ???
}
