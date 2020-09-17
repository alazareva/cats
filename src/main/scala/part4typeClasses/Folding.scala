package part4typeClasses

import cats.{Eval, Monoid}

object Folding extends App {

  // Ex
  object ListExercises {
    def map[A, B](list: List[A])(f: A => B): List[B] = list.foldRight(List.empty[B]){
      (a, acc) => f(a) :: acc
    }
    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = list.foldLeft(List.empty[B]){
      (acc, a) => acc ++ f(a)
    }
    def filter[A](list: List[A])(predicate: A => Boolean): List[A] = list.foldRight(List.empty[A]){
      (a, acc) => if (predicate(a)) a :: acc else acc
    }
    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A = list.foldLeft(monoid.empty)(
      (acc, a) => monoid.combine(acc, a)
    )
  }

  println(ListExercises.map((1 to 4).toList)(_ + 1))
  println(ListExercises.flatMap((1 to 4).toList)(i => List(i + 1)))
  println(ListExercises.filter((1 to 4).toList)(_ % 2 == 0))

  import cats.instances.int._
  println(ListExercises.combineAll((1 to 4).toList))

  import cats.Foldable
  import cats.instances.list._

  val sum = Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _)

  import cats.instances.option._

  val sumOption = Foldable[Option].foldLeft(Option(2), 20)(_ + _) // 22

  // stack safe because it's using eval
  val sumRight = Foldable[List].foldRight(List(1, 2, 3), Eval.now(0)) {
    (num, eval) => eval.map(_ + num)
  }.value

  val anotherSum = Foldable[List].combineAll(List(1, 2, 3)) // needs implicit monoid int

  import cats.instances.string._
  val mappedConcat = Foldable[List].foldMap(List(1, 2, 3))(_.toString)
  println(mappedConcat)

  val intsNested = List(Vector(1, 2, 3), Vector(1, 3, 4))
  import cats.instances.vector._

  val all = (Foldable[List] compose Foldable[Vector]).combineAll(intsNested)
  println(all)

  // Extension methods

  import cats.syntax.foldable._

  val sum3 = List(1, 2, 3).combineAll
  val mappedConcat2 = List(1, 2, 3).foldMap(_.toString)
}
