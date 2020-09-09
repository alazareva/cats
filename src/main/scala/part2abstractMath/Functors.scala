package part2abstractMath

import scala.util.Try

object Functors extends App {
  // generalize idea of map class

  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  import cats.instances.list._
  import cats.instances.option._
  import cats.instances.try_._
  val listFunctor = Functor[List]
  val optionFunctor = Functor[Option]
  val tryFunctor = Functor[Try]

  val incrementNumbers = listFunctor.map(List(1, 2, 3))(_ + 1)
  val incrementOption = optionFunctor.map(Option(2))(_ + 1)
  val incrementTry = tryFunctor.map(Try(1))(_ + 1)

  // generalizing an api
  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)
  def do10xOption(option: Option[Int]): Option[Int] = option.map(_ * 10)

  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] =
    functor.map(container)(_ * 10)

  println(do10x(List(1, 2, 3)))
  println(do10x(Option(1)))

  // Ex 1 define functor for binary tree

  trait Tree[+T]
  object Tree {
    def leaf[T](value: T) = Leaf(value)
    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(value, left, right)
  }
  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  implicit object TreeFunctor extends Functor[Tree] {

    override def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
      case Leaf(v) => Leaf(f(v))
      case Branch(v, l, r) => Branch(f(v), map(l)(f), map(r)(f))
    }
  }
  println(do10x(Tree.branch(2, Tree.leaf(1),  Tree.leaf(3))))

  // extension methods

  import cats.syntax.functor._

  val tree: Tree[Int] = Tree.branch(40, Tree.branch(5, Tree.leaf(10), Tree.leaf(30)), Tree.leaf(3))

  val incTree = tree.map(_ + 1)

  // Ex write shorter do 10 version by using ext methods

  def do10xShorter[F[_] : Functor](container: F[Int]): F[Int] =
    container.map(_ * 10)

}
