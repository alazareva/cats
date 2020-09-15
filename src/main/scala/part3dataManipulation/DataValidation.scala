package part3dataManipulation

import cats.kernel.Semigroup

import scala.util.Try


object DataValidation extends App {

  import cats.data.Validated

  val aValidValue: Validated[String, Int] = Validated.valid(1)
  val anInvalidValue: Validated[String, Int] = Validated.invalid("Wrong")


  val aTest: Validated[String, Int] = Validated.cond(42 > 39, 99, "Wrong")

  // Ex use Either must be prime, non-negative n <= 100, must be even
  (2 to Math.floor(Math.sqrt(100)).toInt).exists(d => 100 % d == 0)


  def primeTest(n: Int): Boolean = {
    def tailrecPrime(d: Int): Boolean =
    if (d <= 1) true
    else n % d != 0 && tailrecPrime(d - 1)

    if (n == 0 || n == 1 || n == -1) false
    else tailrecPrime(Math.abs(n / 2))
  }

  def testNumber(n: Int): Either[List[String], Int] = {
      val errors = List(
        if (n % 2 == 0) None else Some("Not even"),
        if (n <= 100) None else Some("Not <= 100"),
        if (n >= 0) None else Some("Not non-negative"),
        if (primeTest(n)) None else Some("Not prime")
      ).flatten

    errors match {
      case Nil => Right(n)
      case e => Left(e)
    }
  }

  import cats.instances.list._
  implicit val combineIntMax: Semigroup[Int] = Semigroup.instance[Int](Math.max)

  def validateNumber(n: Int): Validated[List[String], Int] = {
    Validated.cond(n % 2 == 0, n, List("Not even"))
      .combine(Validated.cond(n >= 0, n, List("Not non negative")))
      .combine(Validated.cond(n <= 100, n, List("Not <= 100")))
      .combine(Validated.cond(primeTest(n), n, List("Not prime")))
  }

  // chain

  anInvalidValue.andThen(_ => anInvalidValue)

  // ensure
  aValidValue.ensure(List("wrong"))(_ % 2 == 0)

  // transform
  aValidValue.map(_ + 1)

  aValidValue.leftMap(_.length)
  aValidValue.bimap(_.length, _ + 1)

  // other ds

  val eitherToValidated: Validated[List[String], Int] = Validated.fromEither(Right(32))

  val optionToValidated: Validated[List[String], Int] = Validated.fromOption(None, List("Nothing here"))

  val tryToValidated: Validated[Throwable, Int] = Validated.fromTry(Try("Wrong".toInt))

  aValidValue.toOption
  aValidValue.toEither

  // Ex form validation

  object FormValidation {

    import cats.instances.string._

    type FormValidation[T] = Validated[List[String], T]

    /*
      Fields are name, email and password must all be specified
      email must contain @
      pw needs to be at least 10 chars
     */

    def validateForm(form: Map[String, String]): FormValidation[String] = {
      val validateName = Validated.fromOption(form.get("name").filter(_.length > 0), List("No name"))
      val validateEmail = Validated.fromOption(form.get("email").filter(_.length > 0), List("No email"))
        .ensure(List("Invalid email"))(_.contains("@"))
      val validatePassword = Validated.fromOption(form.get("password").filter(_.length > 0), List("No password"))
        .ensure(List("Invalid password"))(_.length > 10)

      validateName.combine(validateEmail).combine(validateEmail).combine(validatePassword).map(_ => "Success")
    }
  }

  println(FormValidation.validateForm(Map(
    "name" -> "name",
    "email" -> "foo@bar.com",
    "password" -> "abcdefghijklm"
  )))

  import cats.syntax.validated._

  val aValidThing: Validated[List[String], Int] = 41.valid[List[String]]
  val anError: Validated[String, Int] = "Something went wrong".invalid[Int]

}
