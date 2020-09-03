package part1intro

object CatsIntro extends App {

  // Eq checks if types in equality are the same at compile time
  // part 1 import type class
  import cats.Eq

  // part 2 import TC instances for the type you neeed

  import cats.instances.int._

  // part 3 use the type class api

  val intEquality = Eq[Int]

  val aTypeSafeComparison = intEquality.eqv(1, 3)

  // part 4 use extension methods
  import cats.syntax.eq._

  val anotherTypeSafeComparison = 2 === 3
  val notEqualTypeSafeComparison = 2 =!= 3

  // part 5 extending type class ops to composite types
  import cats.instances.list._ // brings Eq[List[Int]] into scope
  val aListComparison = List(2) === List(3)

  // part 6 - create a type class instance for custom type
  case class ToyCar(model: String, price: Double)

  implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar]({
    (car1, car2) => car1.price == car2.price
  })

  val compareTwoCars = ToyCar("a", 1.99) === ToyCar("a", 1.83)


}
