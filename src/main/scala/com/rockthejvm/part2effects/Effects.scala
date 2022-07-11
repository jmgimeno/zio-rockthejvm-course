package com.rockthejvm.part2effects

import scala.concurrent.Future

object Effects {

  // functional programming
  // EXPRESSIONS
  def combine(a: Int, b: Int): Int = a + b

  // local reasoning = type signature describes the kind of computation that will be performed
  // referential transparency = ability to replace an expression with the value that it evaluates to
  val five = combine(2, 3)
  val five_v2 = 2 + 3
  val five_v3 = 5

  // not all expressions are RT
  // example 1: printing
  val resultOfPrinting: Unit = println("Learning ZIO")
  val resultOfPrinting_v2: Unit = () // not the same

  // example 2: changing a variable
  var anInt = 0
  val changingInt: Unit = (anInt = 42) // side effect
  val changingInt_v2: Unit = () // not the same program

  // side effects are inevitable
  /*
    Effect properties:
    - the type signature describes what kind of computation it will perform
    - the type signature describes the type of VALUE that it will produce
    - if side effects are required, construction must be separated from the EXECUTION
  */

  /*
    Example: Option = possibly absent values
    - type signature describes the kind of computation = a possibly absent value
    - type signature says the computation returns an A, if the computation does produce something
    - no side effects are needed

    => Option is an effect
  */
  val anOption: Option[Int] = Option(42)

  /*
    Example 2: Future
    - describes an asynchronous computation
    - it produces a value of type A, if it finishes and it's successful
    - side effects are required (allocating a jvm thread, ...), construction is NOT SEPARATE from execution

    => Future is NOT an effect
  */
  import scala.concurrent.ExecutionContext.Implicits.global
  val aFuture: Future[Int] = Future(42)

  /*
    Example 3: MyIO

    - describes ANY kind of computation (including those performing side effects)
      - describes a computation which might perform side effects
    - it produces a value of type A if the computation is successful
    - side effects are required, construction IS SEPARATE from execution

    => MyIO IS AN EFFECT !!! (indeed the most general one)
  */
  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] =
      MyIO { () =>
        f(unsafeRun())
      }

    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO { () =>
        f(unsafeRun()).unsafeRun()
      }
  }

  val anIOWithSideEffects = MyIO { () =>
    println("producing effect")
    42
  }

  def main(args: Array[String]): Unit = {
    // If I don't call unsafeRun, the side effects are not produced
    anIOWithSideEffects.unsafeRun()
  }
}
