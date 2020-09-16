package part4typeClasses

import cats.{Applicative, Apply, FlatMap}


object WeakerMonads extends App {

  trait MyFlatMap[M[_]] extends Apply[M] {
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    def ap[A, B](ff: M[A => B])(wa: M[A]): M[B] = {
      flatMap(wa)(a => map(ff)(f => f(a)))
    }

  }

  trait MyMonad[M[_]] extends  Applicative[M] with MyFlatMap[M] {
    override def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => pure(f(a)))
  }

  import cats.syntax.flatMap._ // extension method flatMap
  import cats.syntax.functor._ // map

  def getPairs[M[_]: FlatMap](nums: M[Int], chars: M[Char]): M[(Int, Char)] = for {
    n <- nums
    c <- chars
  } yield (n, c)
}
