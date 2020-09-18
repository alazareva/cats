package part5advanced

import cats.Monoid

object InvariantFunctors extends App {

  trait Crypto[A] { self =>
    def encrypt(value: A): String
    def decrypt(enc: String): A

    def imap[B](back: B => A, forth: A => B): Crypto[B] = new Crypto[B] {
      override def encrypt(value: B): String = self.encrypt(back(value))

      override def decrypt(enc: String): B = forth(self.decrypt(enc))
    }
  }

  def encrypt[A](value: A)(implicit crypto: Crypto[A]): String = crypto.encrypt(value)

  def decrypt[A](enc: String)(implicit crypto: Crypto[A]): A = crypto.decrypt(enc)

  implicit val caesarCypher: Crypto[String] = new Crypto[String] {
    override def encrypt(value: String): String = value.map(c => (c + 2).toChar)
    override def decrypt(enc: String): String = enc.map(c => (c - 2).toChar)
  }

  val enc = encrypt("foo")
  val dec = decrypt[String](enc)
  println(enc)
  println(dec)

  implicit val doubleCrypto: Crypto[Double] = caesarCypher.imap(_.toString, _.toDouble)

  implicit val optionCrypto: Crypto[Option[String]] = caesarCypher.imap(_.getOrElse(""), Option(_))

  println(encrypt(Math.PI))
  println(encrypt(Option("FOO")))


  implicit def getOptionCrypto[T](implicit crypto: Crypto[T], m: Monoid[T]): Crypto[Option[T]] =
    crypto.imap(_.getOrElse(m.empty), Option(_))

  import cats.instances.double._
  println(encrypt(Option(Math.PI)))

  import cats.Invariant
  import cats.Show
  import cats.instances.string._

  val showString = Show[String]
  val showOptionString: Show[Option[String]] = Invariant[Show].imap(showString)(Option(_))(_.getOrElse(""))

  import cats.syntax.invariant._

  val showOptionString2: Show[Option[String]] = showString.imap(Option(_))(_.getOrElse(""))

  trait MyInvariant[W[_]] {
    def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B]
  }

  trait MyContravariant[W[_]] extends MyInvariant[W] {
    def contramap[A, B](wa: W[A])(back: B => A): W[B]
    def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] = contramap(wa)(back)
  }

  trait MyFunctor[W[_]] extends MyInvariant[W] { // Covariant functor
    def map[A, B](wa: W[A])(f: A => B): W[B]
    def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] = map(wa)(forth)

  }

}
