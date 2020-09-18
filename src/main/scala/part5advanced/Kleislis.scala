package part5advanced

object Kleislis extends App {

  val func1: Int => Option[String] = x => if (x % 2 == 0) Some("Even") else None
  val func2: Int => Option[Int] = x => Some(x + 1)

  val plainFunc1: Int => String = x => if (x % 2 == 0) "Even" else "fail"
  val plainFunc2: Int => Int = x => x + 1

  val plainFunc3: Int => String = plainFunc2 andThen plainFunc1

  import cats.data.Kleisli
  import cats.instances.option._

  val func1k: Kleisli[Option, Int, String] = Kleisli(func1)
  val func2k: Kleisli[Option, Int, Int]  = Kleisli(func2)

  val func3k: Kleisli[Option, Int, String] = func2k andThen func1k

  val multiply = func2k.map(_ * 2) // x => Option(...).map(_ * 2)

  val chain = func3k.flatMap(_ => func1k)

  import cats.Id
  type InterestingKleisli[A, B] = Kleisli[Id, A, B]

  val times2 = Kleisli[Id, Int, Int](_ * 2)
  val plus4 = Kleisli[Id, Int, Int](_ + 4)
  val composed = for {
    t2 <- times2
    p4 <- plus4
  } yield t2 + p4

  // dependency injection same as Reader[Int, Int]
  println(composed(2)) // times2(2) + plus4(2) == 10

}
