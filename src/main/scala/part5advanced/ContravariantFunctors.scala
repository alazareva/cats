package part5advanced

import cats.Monoid

object ContravariantFunctors extends App {

  trait Format[T]{ self =>
    def format(value: T): String

    def contramap[A](func: A => T): Format[A] = new Format[A] {
      override def format(value: A): String = self.format(func(value))
    }
  }

  def format[A](value: A)(implicit f: Format[A]): String = f.format(value)

  implicit object StringFormat extends Format[String] {
    override def format(value: String): String = "\"" + value + "\""
  }

  implicit object IntFormat extends Format[Int] {
    override def format(value: Int): String = value.toString
  }

  implicit object BoolFormat extends Format[Boolean] {
    override def format(value: Boolean): String = if (value) "True" else "False"
  }

  println(format("Regular"))
  println(format(1))
  println(format(true))

  // problem: given Format[MyType] can we have a Format[Option[MyType]]

  implicit def getOptionFormat[T](implicit f: Format[T], m: Monoid[T]): Format[Option[T]] =
    f.contramap[Option[T]](_.getOrElse(m.empty))

  /*
  IntFormat
  fo: Format[Option[Int]] = IntFormat.contramap[Option[Int]](_.get)
  fo2: Format[Option[Option[Int]]] = fo.contramap[Format[Option[Option[Int]]]](_.get)

  fo2 = IntFormat
       .contramap[Option[Int]](_.get)
       .contramap[Format[Option[Option[Int]]]](_.get)

    order of operations = second get => first get => format of int

    Stack order.

    type classes like this are called Contravariant
   */

  import cats.Show
  import cats.Contravariant
  import cats.instances.int._
  val showInts = Show[Int]
  val showOption: Show[Option[Int]] = Contravariant[Show].contramap(showInts)(_.getOrElse(0))

  import cats.syntax.contravariant._

  def showOptionsShorter: Show[Option[Int]] = showInts.contramap(_.getOrElse(0))
}
