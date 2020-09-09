package part2abstractMath

import java.util.concurrent.Executors

import scala.concurrent.{ExecutionContext, Future}

object Monads extends App {

  // lists
  val numberList = List(1, 2, 3)
  val charsList = List('a', 'b', 'c')

  // create all combos

  val combinationsList = for {n <- numberList; c <- charsList} yield (n, c)

  val numOption = Option(2)
  val charOption = Option('d')

  val  combinationsOption = for {n <- numOption; c <- charOption} yield (n, c)

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(
    Executors.newFixedThreadPool(8)
  )

  val numFuture = Future(42)
  val charFuture = Future('z')

  val combinationFuture = for {n <- numFuture; c <- charFuture} yield (n, c)

  /*
  Pattern
  - wrapping value into an M value
  - the flat map mechanism
   */

  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => pure(f(a)))
  }

  // cats monad

  import cats.Monad
  import cats.instances.option._
  import cats.instances.list._

  val optionMonad = Monad[Option]
  val anOption = optionMonad.pure(4)
  val aTransformedOption = optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None)
  val listMonad = Monad[List]
  val aList = listMonad.pure(3)
  val aTransformedList = listMonad.flatMap(aList)(x => List(x, x + 1))

  // Ex, use monad for Future

  import cats.instances.future._
  val futureMonad = Monad[Future]
  val aFuture = futureMonad.pure(22)
  val aTransformedFuture = futureMonad.flatMap(aFuture)(x => Future(x + 1))

  // specialized API

  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] =
    monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))

  println(getPairs(numberList, charsList))
  println(getPairs(numOption, charOption))
  getPairs(numFuture, charFuture).foreach(println)

  // extension methods
  import cats.syntax.applicative._

  val oneOption = 1.pure[Option] // wraps Some(1)
  val oneList = 1.pure[List]

  import cats.syntax.flatMap._
  import cats.syntax.functor._

  val oneOptionTransformed = oneOption.flatMap(x => (x + 1).pure[Option])

  // Ex implement a shorter version of get pairs

  def getPairsFor[M[_] : Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] =
    for {
      a <- ma
      b <- mb
    } yield (a, b)

  println(getPairsFor(numOption, charOption))

}
