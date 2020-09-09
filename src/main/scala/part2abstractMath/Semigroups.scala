package part2abstractMath

object Semigroups extends App {
  // Semigroups combine elements of the same type

  import cats.Semigroup
  import cats.instances.int._

  val naturalIntSemigroup = Semigroup[Int]
  val intCombination = naturalIntSemigroup.combine(2, 46) // addition
  println(intCombination)

  import cats.instances.string._

  val naturalStringSemigroup = Semigroup[String]
  val stringCombination = naturalStringSemigroup.combine("cats", " and dogs")
  println(stringCombination)

  def reduceInts(list: List[Int]): Int = list.reduce(naturalIntSemigroup.combine)

  println(reduceInts(List(1, 2, 3)))

  // generic api
  def reduceThings[T: Semigroup](list: List[T])(implicit semigroup: Semigroup[T]): T =
    list.reduce(semigroup.combine)

  println(reduceThings(List(1, 2, 3, 4)))

  import cats.instances.option._

  val numberOptions = List(1, 2, 3).map(Option.apply)

  println(reduceThings(numberOptions))

  // Ex 1: support a new type for a semigroup

  case class Expense(id: Long, amount: Double)

  implicit val expenseSemigroup: Semigroup[Expense] = Semigroup.instance[Expense] {
    (ex1, ex2) => Expense(Math.max(ex1.id, ex2.id) + 1, ex1.amount + ex2.amount)
  }
  println(reduceThings(List(Expense(1L, 22), Expense(2L, 11))))

  // extension methods  - |+|

  import cats.syntax.semigroup._

  val anIntSum = 2 |+| 2 // requires an implicit semigroup
  val expenseSum = Expense(1L, 22) |+| Expense(2L, 222)

  // Ex 2: implement reduceThings with |+|
  def reduceThings2[T: Semigroup](list: List[T]): T = list.reduce(_ |+| _)
}
