package com.rockthejvm.part1recap

object CatsTypeClasses extends App {

  /* Type classes that we are going to discuss
     - applicative
      - functor
      - FlatMap
      - monad
      - apply
      - applicativeError/monadError
      - traverse
   */

  // functor - "mappable" data structures
  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  import cats.instances.list._ // import the functor for List
  val listFunctor = Functor[List]

  // generalizable "mapping" APIs
  def increment[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] =
    functor.map(container)(_ + 1)

  import cats.syntax.functor._ // extension methods for functors
  // we are using the short-hand notation for the functor (F[_] : Functor, or in other words, F[_] has a Functor in scope)
  def incrementV2[F[_]: Functor](container: F[Int]): F[Int] = container.map(_ + 1)


  // applicative - "apply" values in the context - the ability to "wrap" types
  trait MyApplicative[F[_]] {
    def pure[A](value: A): F[A]
  }

  import cats.Applicative
  val applicativeList = Applicative[List]
  val aSimpleList = applicativeList.pure(2) // List(2)

  import cats.syntax.applicative._ // extension methods for applicatives
  val aSimpleListV2 = 2.pure[List] // List(2)

  // FlatMap - "flattening" nested contexts - ability to chain multiple operations
  trait MyFlatMap[F[_]] extends MyFunctor[F] { // explained in the previous cats course
    def flatMap[A, B](container: F[A])(f: A => F[B]): F[B]
  }

  import cats.FlatMap
  val flatMapList = FlatMap[List]
  val flatMappedList = flatMapList.flatMap(List(1, 2, 3))(x => List(x, x + 1))
  import cats.syntax.flatMap._
  val flatMappedListV2 = List(1, 2, 3).flatMap(x => List(x, x + 1))
  def crossProduct[F[_]: FlatMap, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    fa.flatMap(a => fb.map(b => (a, b)))

  // monad - "pure" + "flatMap" - applicative + flatMap
  trait MyMonad[F[_]] extends MyApplicative[F] with MyFlatMap[F] {
    override def map[A, B](initialValue: F[A])(f: A => B): F[B] = flatMap(initialValue)(a => pure(f(a)))
  }

  import cats.Monad
  val monadList = Monad[List]
  val aSimpleListV3 = monadList.pure(2) // List(2)
  val flatMappedListV3 = monadList.flatMap(List(1, 2, 3))(x => List(x, x + 1))
  def crossProductV2[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    fa.flatMap(a => fb.map(b => (a, b))) // Monad[List].flatMap(fa)(a => Monad[List].map(fb)(b => (a, b))
  def crossProductV3[F[_]: Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)

  // error-handling type classes
  // some Type classes are more useful than other, for sure one of them is Monad
  // and ApplicativeError or MonadError
  trait MyApplicativeError[F[_], E] extends MyApplicative[F] {
    def raiseError[A](e: E): F[A]
  }

  import cats.ApplicativeError
  // in this case the desirable type is A and the error type is String
  type ErrorOr[A] = Either[String, A]
  // the undesirable type is String, so we need to specify the desirable type, and
  // the error type is String, like the error type defined in the type alias
  val applicativeErrorEither = ApplicativeError[ErrorOr, String]
  val desirableValue = applicativeErrorEither.pure(42) // Right(42)
  val failedValue: ErrorOr[Int] = applicativeErrorEither.raiseError("Bad things happened") // Left("Bad things happened")

  import cats.syntax.applicativeError._ // extension methods for applicative errors
  val desirableValueV2 = 42.pure[ErrorOr] // Right(42)
  val undersirableValueV2: ErrorOr[Int] = "Bad things happened".raiseError // Left("Bad things happened")

  trait MyMonadError[F[_], E] extends MyMonad[F] with MyApplicativeError[F, E] {
    def ensure[A](fa: F[A])(error: E)(predicate: A => Boolean): F[A]
  }

  import cats.MonadError
  val monadErrorEither = MonadError[ErrorOr, String]


  // traverse - "iterating" over contexts
  trait MyTraverse[F[_]] extends MyFunctor[F] {
    def traverse[G[_]: MyApplicative, A, B](container: F[A])(f: A => G[B]): G[F[B]]
  }

  // turn nested wrappers inside out
  val listOfOptions: List[Option[Int]] = List(Some(1), Some(2), Some(3))
  import cats.Traverse
  import cats.syntax.traverse._
  val listTraverse = Traverse[List]
  val anotherOptionsList = listTraverse.traverse(listOfOptions)(x => Option(x))
  val optionList: Option[List[Int]] = listTraverse.traverse(listOfOptions)(x => x)



}
