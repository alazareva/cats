package part2abstractMath

object Monoids extends App {

  import cats.Semigroup
  import cats.instances.int._
  import cats.instances.string._
  import cats.syntax.semigroup._

  val nums = (1 to 1000).toList
  // |+| is always associative
  val sumLeft = nums.foldLeft(0)(_ |+| _)
  val sumRight = nums.foldRight(0)(_ |+| _)

  println(sumLeft)
  println(sumRight)

  // general api, Monoid has zero value

  import cats.Monoid

  val intMonoid = Monoid[Int]

  val combineInt = intMonoid.combine(1, 2)
  val zero = intMonoid.empty // 0

  import cats.instances.option._

  val combineOption = Monoid[Option[Int]].combine(Option(2), Option.empty[Int])

  def combineFold[T](list: List[T])(implicit monoid: Monoid[T]): T =
    list.foldLeft(monoid.empty)(_ |+| _)

  println(combineFold(nums))

  // Ex combine a list of phone books

  val phoneBooks = List(
    Map(
      "Alice" -> 111,
    ),
    Map(
      "Bob" -> 222,
      "Dog" -> 211,
    )
  )

  import cats.instances.map._

  val combinedMaps = combineFold(phoneBooks)
  println(combinedMaps)

  case class ShoppingCart(items: List[String], total: Double)

  implicit val shoppingCartMonoid = new Monoid[ShoppingCart] {
    override val empty = ShoppingCart(List.empty, 0)

    override def combine(x: ShoppingCart, y: ShoppingCart): ShoppingCart =
      ShoppingCart(x.items ++ y.items, x.total + y.total)

  }

  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart = combineFold(shoppingCarts)

  println(checkout(List(ShoppingCart(List("apple", "grape"), 22), ShoppingCart(List("fan"), 11))))

}
