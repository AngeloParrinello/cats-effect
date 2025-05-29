package com.rockthejvm.part1recap

object ContextualAbstractionScala3 {

  // we will see three main things here:

  // given/using combo -- equivalent of implicit in Scala 2
  def increment(x: Int)(amount: Int): Int = x + amount
  val twelve = increment(7)(5)

  def usingIncrement(x: Int)(using amount: Int): Int = x + amount
  given amount: Int = 5
  val seventeen = usingIncrement(12) // 5 automatically injected

  def multiply(x: Int, y: Int)(using factor: Int): Int = x * y * factor
  // given factor: Int = 5 // error: ambiguous implicits, two givens with the same type
  val sixty = multiply(3, 4) // 5 automatically injected

  // more complex use case
  trait Combine[A]:
    def combine(x: A, y: A): A
    def empty: A

  // so combineAll will use the given Combine[A] in scope to combine all elements in a single value
  def combineAll[A](list: List[A])(using comb: Combine[A]): A =
    list.foldLeft(comb.empty)(comb.combine)

  /*
  same as below:

  given intCombine: Combine[Int] with
    def combine(x: Int, y: Int): Int = x + y
    def empty: Int = 0
   */
  given Combine[Int] with
    def combine(x: Int, y: Int): Int = x + y
    def empty: Int = 0

  val numbers = (1 to 10).toList
  val sum10 = combineAll(numbers) // int combine is automatically injected
  // combineAll(List["a", "b", "c"]) // error: no Combine[String] in scope

  // with this kind of structure we can define some functionality for certain types and not for others

  // synthesize given instances
  // we want to synthesize a Combine[Option[T]] if we have a Combine[T] aka the compiler can derive a Combine[Option[T]] if it knows how to combine T
  // and this will be the same for String, Int, whatever type we want to combine!!
  given optionCombiner[T](using comb: Combine[T]): Combine[Option[T]] with
    def combine(x: Option[T], y: Option[T]): Option[T] = for {
      a <- x
      b <- y
    } yield comb.combine(a, b)
    def empty: Option[T] = None

    val sumOptions: Option[Int] = combineAll(List(Some(2), None, Some(4)))

  // extension methods (mow supported stand alone)
  case class SinglePerson(name: String):
    def greet: String = s"Hello, I'm $name"

  extension (name: String)
    def greet: String = SinglePerson(name).greet

  // in this way wherever I use a String I can call greet on it!!
  // very useful to decorate existing types with new functionality

  val alicesGreeting = "Alice".greet

  // generic extension methods
  extension [A](list: List[A])
    // it can also take implicit parameters!
    def reduceAll(using comb: Combine[A]): A =
      list.foldLeft(comb.empty)(comb.combine)

  val sum10V2 = numbers.reduceAll // int combine is automatically injected

  // type classes (pattern)
  object TypeClassesScala3:
    case class Person(name: String, age: Int)

    // part 1 - type class definition - define the trait
    trait JSONSerializer[T]:
      def toJson(value: T): String

    // part 2 - type class instances - define the given instances
    given stringSerializer: JSONSerializer[String] with
      def toJson(value: String): String = s""""$value"""" // JSON string

    given JSONSerializer[Int] with
      def toJson(value: Int): String = value.toString // JSON number

    given personSerializer: JSONSerializer[Person] with
      def toJson(value: Person): String = s"""
        |{"name": ${value.name}, "age": ${value.age}}
        |""".stripMargin.trim

    // part 3 - user-facing API
    def convertToJSON[T](value: T)(using serializer: JSONSerializer[T]): String =
      serializer.toJson(value)

    def convertListToJSON[T](list: List[T])(using serializer: JSONSerializer[T]): String =
      list.map(serializer.toJson).mkString("[", ",", "]")

    // part 4 - extension methods just for the types we support
    extension [T](value: T)
      def toJSON(using serializer: JSONSerializer[T]): String = serializer.toJson(value)


  def main(args: Array[String]): Unit =
    import TypeClassesScala3.*
    println(convertListToJSON(List(Person("Alice", 23), Person("Bob", 25))))
    val bob  = Person("Bob", 25)
    println(bob.toJSON)



}
