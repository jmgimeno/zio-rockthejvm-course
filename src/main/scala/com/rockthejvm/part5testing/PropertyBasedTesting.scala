package com.rockthejvm.part5testing

import zio.*
import zio.test.*
import com.rockthejvm.utils.debugThread

object PropertyBasedTesting extends ZIOSpecDefault:

  // "proofs"
  // for all x, y, z, we have (x + y) + z = x + (y + z)
  // shrinking
  def spec = test("property based testing basics") {
    check(Gen.int, Gen.int, Gen.int) { (x, y, z) =>
      assertTrue((x + y) + z == x - (y + z))
    }
  }

  /*
    Property-based testing
      "for all x, y, z, ... we have Statement to be true"

    translates to
      check(generators) { (x, y, z, ...) =>
        assertions on the generated values
      }
   */

  // Gen[R, A]; R = Environment; A = value
  val intGenerator = Gen.int
  // also alphaChar, alphaNumericChar, asciiChar, hexChar, printableChar, ...
  val charGenerator               = Gen.char
  val stringGenerator             = Gen.string
  val cappedLengthStringGenerator = Gen.stringN(10)(Gen.alphaNumericChar)
  val constGenerator              = Gen.const("Scala")
  val valuesGenerator             = Gen.elements(1, 3, 5, 7, 9)
  val valuesIterableGenerator     = Gen.fromIterable(1 to 1000)
  val uniformDoublesGenerator     = Gen.uniform // select doubles between 0 and 1
  // list generator
  val listGenerator      = Gen.listOf(Gen.string)  // unbounded list of strings
  val finiteSetGenerator = Gen.setOfN(10)(Gen.int) // sets of 10 integers
  // option, either
  val optionGenerator = Gen.option(Gen.int)
  val eitherGenerator = Gen.either(Gen.string, Gen.int)
  // combinators
  val zippedGenerator     = Gen.int.zip(Gen.string)
  val filteredGenerator   = intGenerator.filter(_ % 3 == 0)
  val mappedGenerator     = Gen.int.map(n => (1 to n).map(_ => 'a').mkString)
  val flatMappedGenerator = filteredGenerator.flatMap(l => Gen.stringN(l)(Gen.alphaNumericChar))
  // for-comprehension
  // 8-4-4-12
  val uuidGenerator =
    for
      part1 <- Gen.stringN(8)(Gen.alphaNumericChar)
      part2 <- Gen.stringN(4)(Gen.alphaNumericChar)
      part3 <- Gen.stringN(4)(Gen.alphaNumericChar)
      part4 <- Gen.stringN(12)(Gen.alphaNumericChar)
    yield s"$part1-$part2-$part3-$part4"

  // general
  val randomGenerator = Gen.fromRandom(random => random.nextUUID)
  val effectGenerator = Gen.fromZIO(ZIO.succeed(42))
  // list of strings with the property that every string will have increasing length
  val generalGenerator =
    Gen.unfoldGen(0)(i => Gen.const(i + 1).zip(Gen.stringN(i)(Gen.alphaNumericChar)))

object GenerationPlayGround extends ZIOAppDefault:
  def run =
    val generalGenerator =
      Gen.unfoldGen(0)(i => Gen.const(i + 1).zip(Gen.stringN(i)(Gen.alphaNumericChar)))
    val generatedListsZIO = generalGenerator.runCollectN(100)
    // It seems that now Sized is not a requirement for some generators
    // val generatedListsZIO_v2 = generatedListsZIO.provideLayer(Sized.default)
    generatedListsZIO.debugThread
