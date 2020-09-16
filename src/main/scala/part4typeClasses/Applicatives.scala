package part4typeClasses


object Applicatives extends App {
  // Functors + pure method

  import cats.Applicative

  import cats.instances.list._

  val listApplicative = Applicative[List]
  val aList = listApplicative.pure(2)

  import cats.instances.option._

  val option = Applicative[Option].pure(2)

  // pure extension methods

  import cats.syntax.applicative._

  val anotherList = 2.pure[List]
  val anotherOption = 2.pure[Option]

  // Monads extend applicative
  // Applicatives extend Functors

  import cats.data.Validated

  type ErrorsOr[T] = Validated[List[String], T]

  val aValidValue: ErrorsOr[Int] = Validated.valid(32) // "pure"
  val aModifiedValidated = aValidValue.map(_ + 1) // map

  val validatedApplicative = Applicative[ErrorsOr]

  // you should use validated for Functors that have map and have a pure method but are NOT monads

  // Ex

  def ap[F[_], A, B](ff: F[A => B])(fa: F[A]): F[B] = ???
  def productWithApplicatives[F[_], A, B](fa: F[A], fb: F[B])(implicit applicative: Applicative[F]): F[(A, B)] = {
   val ff =  applicative.map(fa)(a => (b: B) => (a, b))
    applicative.ap(ff)(fb)
  }

  // Applicatives have ap[F[_], A, B](ff: F[A => B])(fa: F[A]): F[B] and can implement product from
  // Semigroupal so they extend Semigroupal

}
