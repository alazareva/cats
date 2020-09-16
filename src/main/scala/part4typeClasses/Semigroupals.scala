package part4typeClasses

import cats.Monad

object Semigroupals extends App {

  trait MySemigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }


  import cats.Semigroupal

  import cats.instances.option._

  val optionSemigroupal = Semigroupal[Option]

  val aTupledOption = optionSemigroupal.product(Some(122), Some("s")) // Some((122, 's'))

  val aNoneTupled = optionSemigroupal.product(None, Some(1)) // None

  import cats.instances.list._
  val aTupledList = Semigroupal[List].product(List(1, 2), List("a", "b"))

  println(aTupledList)

  // Ex
  import cats.syntax.functor._
  import cats.syntax.flatMap._

  def productWithMonads[F[_] : Monad, A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
   for {
     a <- fa
     b <- fb
   } yield (a, b)
  }

  // Monads extend semigroupal

  import cats.data.Validated

  type ErrorOr[T] = Validated[List[String], T]

  val validatedSemigroupal = Semigroupal[ErrorOr]

  val invalidCombination = validatedSemigroupal.product(
    Validated.invalid(List("Something wrong", "Something else wrong")),
    Validated.invalid(List("This is not right"))
  )

  type EitherErrorsOr[T] = Either[List[String], T]

  import cats.instances.either._ // imports monad

  val eitherSemigroupal = Semigroupal[EitherErrorsOr]

  val eitheCombination = eitherSemigroupal.product(
    Left(List("Something wrong", "Something else wrong")),
    Left(List("This is not right"))
  )
  println(invalidCombination) // combines errors
  println(eitheCombination) // does not combine errors

  // define a Semigroupal of List that does a zip

  val myListSemigroupal = new Semigroupal[List] {
    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = (fa, fb) match {
      case (Nil, _) | (_, Nil) => Nil
      case (h1 :: t1, h2 :: t2) => (h1, h2) :: product(t1, t2)
    }
  }

  println(myListSemigroupal.product(List("A", "B"), List(1, 2)))
}
