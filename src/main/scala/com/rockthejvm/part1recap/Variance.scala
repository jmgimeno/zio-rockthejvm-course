package com.rockthejvm.part1recap

object Variance {

  // OOP - substitution
  class Animal
  class Dog(name: String) extends Animal

  // Variance question for List
  // if Dog <: Animal, the should List[Dog] <: List[Animal]?
  // YES - COVARIANT

  val lassie = new Dog("Lassie")
  val hachi = new Dog("Hachi")
  val laika = new Dog("Laika")

  val anAnimal: Animal = lassie
  val someAnimal: List[Animal] = List(lassie, hachi, laika)

  class MyList[+A] // MyList is COVARIANT in A
  val myAnimalList: MyList[Animal] = new MyList[Dog]

  // NO - INVARIANT
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  // all generics in Java
  // val aJavaList: java.util.ArrayList[Animal] = new java.util.ArrayList[Dog]()

  // HELL NO - CONTRAVARIANCE
  trait Vet[-A] {
    def heal(animal: A): Boolean
  }

  // Vet[Animal] is "better" than a Vet[Dog]; she/he can treat ANY animal, therefore can treat my dos as well
  // Dog <: Animal, then Vet[Dog] >: Vet[Animal]
  val myVet: Vet[Dog] = new Vet[Animal] {
    override def heal(animal: Animal): Boolean = {
      println("Here you go, you're good to go ...")
      true
    }
  }

  val healingLassie = myVet.heal(lassie)

  /*
    Rule of thumb:
    - if the type PRODUCES or RETRIEVES values of type A (e.g. lists), then the type should be COVARIANT
    - if the type CONSUMES or ACTS ON values of type A (e.g. a vet), then the type should be CONTRAVARIANT
    - otherwise, INVARIANT
  */

  /*
    Variance positions

    class class Cat extends Animal
    class Vet2[-A](val favoriteAnimal: A) // contravariant type A occurs in covariant position in type A of value favoriteAnimal
      // the types of val fields are in COVARIANT position
    val garfield = new Cat
    val theVet: Vet2[Animal] = new Vet2[Animal](garfield)
    val dogVet: Vet2[Dog] = theVet
    val favAnimal: Dog = dogVet.favAnimal // must be a dog but this is a cat !!!
  */

  // var fields are also in COVARIANT position (same as vals)

  /*
    class MutableContainer[+A](var contents: A) // <-- types of vars are in a CONTRAVARIANT position

    val containerAnimal: MutableContainer[Animal] = new MutableContainer[Dog](new Dog)
    containerAnimal.contents = new Cat // type conflict !!!
  */

  // so vars must be COVARIANT + CONTRAVARIANT => INVARIANT

  // types of arguments are in CONTRAVARIANT position

  /*
    class MyList2[+A] {
      def add(element: A): MyList2[A] // covariant type A occurs in contravariant position in type A of parameter element
    }

    val animals: MyList2[Animal] = new MyList2[Cat]
    val biggerListOfAnimals: MyList2[Animal] = animals.add(new Dog) // type conflict
  */

  // solution
  class MyList2[+A] {
    def add[B >: A](element: B): MyList[B] = ??? // the result is of a wider type
  }

  // method return types are in a COVARIANT position
  /*
    abstract class Vet2[-A] {
      def rescueAnimal(): A // contravariant type A occurs in covariant position in type (): A of method recueAnimal
    }

    val vet: Vet2[Animal] = new Vet2[Animal] {
      def rescueAnimal(): Animal = new Cat
    }
    val lassieVet: Vet2[Dog] = vet
    val rescueDog: Dog = lassieVet.rescueAnimal() // must return a dog but returns a cat - type conflict !!!
  */

  abstract class Vet2[-A] {
    def rescueDog[B <: A](): B // the result if of a narrower type
  }
  def main(args: Array[String]): Unit = {

  }

}
