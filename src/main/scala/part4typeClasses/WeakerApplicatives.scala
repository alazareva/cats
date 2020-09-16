package part4typeClasses

import cats.{Functor, Semigroupal}

object WeakerApplicatives extends App {

  trait MyApply[F[_]] extends Functor[F] with Semigroupal[F] {
    def ap[A, B](ff: F[A => B])(fa: F[A]): F[B] = ???

    override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
      val ff =  map(fa)(a => (b: B) => (a, b))
      ap(ff)(fb)
    }

    def mapN[A, B, C](tuple: (F[A], F[B]))(f: (A, B) => C): F[C] = {
      val prod = product(tuple._1, tuple._2)
      map(prod)(t => f(t._1, t._2))
    }
  }

  trait MyApplicative[F[_]] extends MyApply[F] {
    def pure[A](x: A): F[A]
  }

  import cats.Apply
  import cats.instances.option._

  val applyOption = Apply[Option]

  val funcApp = applyOption.ap(Some((x: Int) => x + 1))

  import cats.syntax.apply._ // extension methods

  val tupleOfOptions = (Option(1), Option(2), Option(3))

  val optionOfTuples = tupleOfOptions.tupled // Some((1, 2, 3))

  val sumOption = tupleOfOptions.mapN(_ + _ + _) // Some(6)

}
