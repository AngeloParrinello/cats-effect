package com.rockthejvm.part1recap

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object Essentials {

  // values
  val aBoolean: Boolean = false // immutable

  // expressions are EVALUATED to a value (reduced to a value)
  val anIfExpression = if (2 > 3) "bigger" else "smaller"

  // instructions vs expressions --> side effects, do not compute any value and show some effect
  val theUnit: Unit = println("Hello, Scala") // Unit = "void" in other languages = ()

  // OOP
  class Animal
  class Cat extends Animal
  trait Carnivore {
    def eat(animal: Animal): Unit
  }

  // inheritance + polymorphism (inheritance model, you can extend at most one class but inherit from multiple traits)
  class Crocodile extends Animal with Carnivore {
    override def eat(animal: Animal): Unit = println("Crunch!")
  }

  // singleton pattern
  object MySingleton

  // companions (AN OBJECT WITH THE SAME NAME AS A CLASS)
  object Carnivore // companion object OF the trait Carnivore

  // generics
  class MyList[A]

  // method notation (infix method notation)
  val three = 1 + 2
  val anotherTree = 1.+(2) // equivalent

  // functional programming
  // function types: Function1[A, B], Function2[A, B, C], ... Function22
  val incrementer: Int => Int = new Function1[Int, Int] {
    override def apply(v1: Int): Int = v1 + 1
  }
  val anotherIncrementer: Int => Int = (x: Int) => x + 1
  val incremented = incrementer(42)

  // Higher-order functions: take functions as args or return functions as results
  // map, flatMap, filter in the collections
  val processedList = List(1, 2, 3).map(incrementer) // List(2, 3, 4)
  val flatMapped = List(1, 2, 3).flatMap(x => List(x, x + 1)) // List(1, 2, 2, 3, 3, 4)

  // options and try
  val anOption: Option[Int] = Option(2) // Some(2)
  val doubledOption = anOption.map(_ * 2) // Some(4)

  val anAttempt = Try {
    throw new RuntimeException("I'm innocent, I swear!") // Failure(RuntimeException)
  }
  val anotherAttempt = Try {
    42
  }
  val aModifiedAttempt = anotherAttempt.map(x => x + 2) // Success(44)

  // pattern matching
  val aPatternMatchedValue = anOption match {
    case Some(2) => 42
    case _ => 0
  }

  // Futures
  implicit val ec: scala.concurrent.ExecutionContext = ExecutionContext.fromExecutorService(java.util.concurrent.Executors.newFixedThreadPool(8))
  val aFuture = Future(42)
  // wait for the future to complete (async)
  aFuture.onComplete {
    case Success(value) => println(s"The async computation yielded $value")
    case Failure(exception) => println(s"The async computation failed with $exception")
  }

  // map a Future
  val anotherFuture = aFuture.map(_ + 1) // Future(43) when the original future completes

  // for-comprehensions
  val checkerboard = List(1, 2, 3).flatMap(n => List('a', 'b', 'c').map(c => s"$n-$c"))
  val anotherCheckerboard = for {
    n <- List(1, 2, 3)
    c <- List('a', 'b', 'c')
  } yield s"$n-$c"

  // partial function (a function that is not defined for all inputs)
  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 56
    case 3 => 999
  }

  // some more advanced stuff
  trait HigherKindedType[F[_]]
  trait SequenceChecker[F[_]] {
    def check[A](sequence: F[A]): Boolean
  }

  val listChecker = new SequenceChecker[List] {
    override def check[A](sequence: List[A]): Boolean = sequence.size > 1
  }

}
