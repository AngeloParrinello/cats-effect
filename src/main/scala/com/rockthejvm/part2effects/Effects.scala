package com.rockthejvm.part2effects

import scala.concurrent.Future

object Effects extends App {

  // pure functonal prgramming
  // it computes a single value and no side effect are produced
  def combine(a: Int, b: Int): Int = a + b
  val five = combine(2, 3) // 5
  val five_v2 = 2 + 3 // 5
  val five_v3 = 5

  // referential transparency = can replace an expression with its value as many times as we want without changing the behavior
  // above we have 3 examples of referential transparency. This is a core part of pure functional programming
  // because in the absence of side effects, we can replace the expression with its value without changing the behavior

  // but the referential transparency can be broken

  // example: print to console
  val printSomething: Unit = println("I'm printing something")
  val printSomething_v2: Unit = ()
  // the above 2 expressions are not the same, example of referential transparency broken

  // example: change a variable
  var anInt = 0
  val changingVar: Unit = (anInt += 1)
  val changingVar_v2: Unit = () // not the same again

  // but side effects are inevitable in any program

  // so we need something that handle side effects in a pure functional way
  // we need to encapsulate the side effects in a data structure

  // effect
  /*
  Desires or better call as Effect Types properties:
  - type signature describes the kind of calculation that will be performed (i.e. I need to understand what the effect does)
  - type signature describes the value THAT will be calculated (i.e. what kind of value the calculation will produce)
  - when side effects are needed, effect construction is separate from the effect execution
   */

  // example: the Option effect
  // as we know, it describes the possibility of a value being present or not
  // 1) type signature describes the kind of calculation that will be performed (describes the possibility of a value being present or not)
  // 2) type signature describes the value THAT will be calculated (the value that may or may not be present, inside the Option, in this case is Int if it is present)
  // 3) when side effects are needed, effect construction is separate from the effect execution (in this case side effects are not needed)
  // So in this case Option is an effect type!
  val anOption: Option[Int] = Option(42) // Option is a data structure that encapsulates the possibility of a value being present or not

  /*
  example: the Future effect: Future is NOT an effect type
  - describes an asynchronous computation that will produce a value at some point in the future
  - type signature describes the kind of calculation that will be performed (asynchronous computation in this case type Int)
  - BUT a side effect is required (allocating/scheduling a thread), so execution is NOT separate from construction
   */
  import scala.concurrent.ExecutionContext.Implicits.global
  val aFuture: Future[Int] = Future {
    // some long computation
    42
  }

  // example
  /*
  Is this an effect type?
  - describes any computatin tjat might produce side effects
  - calcuates a avalue of type A if it-s successful
  - side effects are required for the evaluation of () => A
  - is the data structure creation separate from the execution? YES, the creation of MyIO does not produce side effects on construction
  - because the unsafeRun method is a function that produces the side effects and it's separate from the construction of MyIO

  So the MyIO is an effect type!!

   */
  case class MyIO[A](unsafeRun: () => A){
    def map[B](f: A => B): MyIO[B] = MyIO(() => f(unsafeRun()))
    def flatMap[B](f: A => MyIO[B]): MyIO[B] = MyIO(() => f(unsafeRun()).unsafeRun())
  }

  val anIO = MyIO(() => {
    println("I'm doing something")
    42
  })
  // this method will only be invoked when we call unsafeRun, but during the creation of anIO, no side effects are produced!
  anIO.unsafeRun()


  /**
   * Exercises:
   * 1: An IO which returns the current time of the system. And this IO will produce the current time of the system only when we call unsafeRun
   * 2. an IO which measures the duration of a computation (hint: use ex 1)
   * 3. an IO which prints something to the console
   * 4. an IO which reads a line (a string) from the console
   */

  // 1
  val currentTime: MyIO[Long] = MyIO(() => System.currentTimeMillis())

  // 2
  // this works but we can do better with for-comprehension
  def measure[A](computation: MyIO[A]): MyIO[Long] =
    computation.flatMap{_ => MyIO(() => {
      val start = System.currentTimeMillis()
      val comp = computation.unsafeRun
      val endTime = System.currentTimeMillis()
      endTime - start
      }
      )
    }

  def betterMeasure[A](computation: MyIO[A]): MyIO[Long] = for {
    start <- currentTime
    _ <- computation
    end <- currentTime
  } yield end - start

  /*
  let's desugarize the for-comprehension
  currentTime.flatMap(start => computation.flatMap(_ => currentTime.map(end => end - start)))

  currentTime.map(end => end - start)) = MyIO(() => currentTime.unsafeRun() - start)

  => currentTime.flatMap(start => computation.flatMap(_ => MyIO(() => currentTime.unsafeRun() - start)))
  computation.flatMap(lambda...) = MyIO(() => lambda(___COMP___).unsafeRun())
                                 = MyIO(() => MyIO(() => currentTime.unsafeRun() - start).unsafeRun())
                                 = MyIO(() => currentTime.unsafeRun() - start) // because unsafeRun() is called on the inner MyIO
                                = MyIO(() => System.currentTimeMillis_after_computation() - start) // because currentTime.unsafeRun() is called after the computation

  => currentTime.flatMap(start => MyIO(() => System.currentTimeMillis_after_computation() - start))
  = MyIO(() => MyIO(() => System.currentTimeMillis_after_computation() - System.currentTimeMillis_before_computation()).unsafeRun())
  = MyIO(() => System.currentTimeMillis_after_computation() - System.currentTimeMillis_before_computation())
   */

  // 3
  val printSomethingElse = MyIO(() => println("Print something else"))
  def putStrLn(line: String): MyIO[Unit] = MyIO(() => println(line))

  // 4
  def readLine: MyIO[String] = MyIO(() => scala.io.StdIn.readLine())

  def testTimeIO(): Unit = {
    val test = measure(MyIO(() => {
      Thread.sleep(1000)
      42
    }))
    println(test.unsafeRun())
  }

  // yielding Unit is very common in functional programming and especially in Cats Effects
  def testConsoleIO(): Unit = {
    val test: MyIO[Unit] = for {
      _ <- putStrLn("Hello, what's your name?")
      name <- readLine
      _ <- putStrLn(s"Hello, $name!")
    } yield ()
    test.unsafeRun()
  }








}
