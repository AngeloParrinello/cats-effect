package com.rockthejvm.part1recap

import scala.concurrent.duration.DurationInt

object Implicits extends App {

  case class Person(name: String) {
    def greet(): String = s"Hi, my name is $name"
  }

  implicit class ImpersonableString(name: String) {
    def greet(): String = Person(name).greet()
  }

  val greeting = "Angelo".greet() // new ImpersonableString("Angelo").greet()
  // this mechanism is called "implicit conversions"
  // example: scala.concurrent.duration
  val oneSecond = 1.second

  // implicit arguments and values
  def increment(x: Int)(implicit amount: Int): Int = x + amount
  val defaultAmount = 10
  val incremented = increment(2)(defaultAmount) // increment(2)(10)

  def multiply(x: Int)(implicit amount: Int): Int = x * amount
  // implicit val defaultMultiplier = 2 // if I define this, the compiler will complain about ambiguous implicits
  val multiplied = multiply(2)(defaultAmount) // multiply(2)(2)

  // more complex example
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  def convertToJson[T](value: T)(implicit serializer: JSONSerializer[T]): String = serializer.toJson(value)

  implicit val personSerializer: JSONSerializer[Person] = new JSONSerializer[Person] {
    override def toJson(value: Person): String = s"""{"name": "${value.name}"}"""
  }

  val davidsJson = convertToJson(Person("David")) // convertToJson(Person("David"))(personSerializer) implicit serializer passed here

  // implicit defs
//  implicit def createListSerializer[T](implicit serializer: JSONSerializer[T]): JSONSerializer[List[T]] = new JSONSerializer[List[T]] {
//    override def toJson(value: List[T]): String = s"[${value.map(convertToJson).mkString(", ")}]"
//  }

  // val personsJson = convertToJson(List(Person("Alice"), Person("Bob"))) // convertToJson(List(Person("Alice"), Person("Bob")))(createListSerializer(personSerializer))

  // implicit conversions (not recommended)
  case class Cat(name: String) {
    def meow(): String = s"Meow, I'm $name"
  }

  implicit def string2Cat(name: String): Cat = Cat(name)
  val cat: Cat = "Luna" // string2Cat("Luna")
  val lunaMeow = "Luna".meow() // string2Cat("Luna").meow()
  // now the method meow is available on strings, this is not the technique to use for enable extension methods!
  // much better to use implicit classes!

}

// main use of implicit is Type Classes!
object TypeClassesScala2 {
  case class Person(name: String, age: Int)

  // part1: type class instance, functionality for a type
  // type class DEFINITION
  // the type class describes the operations that we can apply to a type
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  // part 2 - type class instances (the type class instances for the types we care about, the types we want to enrich, the types that will support a specific functionality)
  // type class INSTANCES
  implicit object StringSerializer extends JSONSerializer[String] {
    override def toJson(value: String): String = s""""$value""""
  }

  implicit object IntSerializer extends JSONSerializer[Int] {
    override def toJson(value: Int): String = value.toString
  }

  implicit object PersonSerializer extends JSONSerializer[Person] {
    override def toJson(value: Person): String = s"""{"name": "${value.name}", "age": ${value.age}}"""
  }

  // part 3 - offer some API, some methods to use the type class
  def convertToJson[T](value: T)(implicit serializer: JSONSerializer[T]): String = serializer.toJson(value)

  def convertListToJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String = list.map(serializer.toJson).mkString("[", ", ", "]")


  // part 4 - add extension methods to the existing types
  // implicit classes
  object JSONSyntax {
    // so in a single import, we can enrich the existing types with the type class functionality
    // we are basically say: for every value T that has a JSONSerializer, I want to add a method toJson
    implicit class JSONSerializable[T](value: T)(implicit serializer: JSONSerializer[T]) {
      def toJson: String = serializer.toJson(value)
    }
  }

  println(convertToJson("Scala")) // StringSerializer.toJson("Scala")
  println(convertToJson(42)) // IntSerializer.toJson(42)
  println(convertToJson(Person("Alice", 29))) // PersonSerializer.toJson(Person("Alice", 29))
  println(convertListToJson(List(Person("Alice", 29), Person("Bob", 33)))) // List(Person("Alice", 29), Person("Bob", 33)).map(PersonSerializer.toJson).mkString("[", ", ", "]")

  // using the implicit class
  import JSONSyntax._
  val bob = Person("Bob", 33)
  println(bob.toJson) // new JSONSerializable(bob).toJson

}
