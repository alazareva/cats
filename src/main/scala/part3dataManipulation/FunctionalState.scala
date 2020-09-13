package part3dataManipulation


object FunctionalState extends App {

  type MyState[S, A] = S => (S, A)

  import cats.data.State

  val countAndSay: State[Int, String] = State(count => (count + 1, s"Counted $count"))

  val (eleven, counted10) = countAndSay.run(10).value

  // state = iterative computation


  // using vars

  var a = 10

  a += 1
  val firstComputation = s"Added 1 got $a"

  a += 5
  val secondComputation = s"Added 4 $a"

  // with FP

  val firstTransformation: State[Int, String] = State((s: Int) => (s + 1, s"Added 1 got ${s + 1}"))
  val secondTransformation: State[Int, String] = State((s: Int) => (s + 5, s"Added 5 got ${s + 5}"))

  val compositeTransformation: State[Int, (String, String)] = firstTransformation.flatMap { first =>
    secondTransformation.map(second => (first, second))
  }

  val compositeTransformation2 = for {
    first <- firstTransformation
    second <- secondTransformation
  } yield (first, second)


  val func1 = (s: Int) => (s + 1, s"Added 1 got ${s + 1}")
  val func2 = (s: Int) => (s + 5, s"Added 5 got ${s + 5}")

  val compositeResult = func1.andThen {
    case (newState, firstResult) => (firstResult, func2(newState))
  }

  println(compositeTransformation.run(10).value)
  println(compositeResult(10)) // nested structure

  // Ex

  case class ShoppingCart(items: List[String], total: Double)

  def addToCart(item: String, price: Double): State[ShoppingCart, Double] =
    State((cart: ShoppingCart) => (ShoppingCart(item :: cart.items, price + cart.total), price + cart.total))

  val myCart = for {
    _ <- addToCart("fender", 200)
    _ <- addToCart("cable", 100)
    total <- addToCart("another", 100)
  } yield total

  println(myCart.run(ShoppingCart(List.empty, 0)).value)

  // Ex

  def inspect[A, B](f: A => B): State[A, B] = State(a => (a, f(a)))

  def get[A]: State[A, A] = State(a => (a, a))

  def set[A](value: A): State[A, Unit] = State(_ => (value, ()))

  def modify[A](f: A => A): State[A, Unit] = State(a => (f(a), ()))

  // Methods already in

  import cats.data.State._

  val program = for {
    a <- get[Int]
    _ <- set[Int](a + 10)
    b <- get[Int]
    _ <- modify[Int](_ + 20)
    t <- inspect[Int, Int](_ * 2)
  } yield (a, b, t)

  println(program.run(1).value)

}
