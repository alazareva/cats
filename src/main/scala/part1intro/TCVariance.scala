package part1intro

object TCVariance extends App {

  import cats.Eq
  import cats.instances.int._
  import cats.instances.option._
  import cats.syntax.eq._

  val aComparison = Option(2) === Option(3)

  // val invalidComparison = Some(2) === None Eq[Some[Int]] not found

  // variance

  class Animal

  class Cat extends Animal

  // covariant type: subtyping is propagated to the generic type
  class Cage[+T]

  val cage: Cage[Animal] = new Cage[Cat] // cat extends animal so Cage[Cat] <: Cage[Animal]

  // contravariant type: subtyping is propagated backwards to the generic type

  class Vet[-T]

  val vet: Vet[Cat] = new Vet[Animal] // Cat <: Animal then Vet[Animal] <: Vet[Cat]

  // if a generic type HAS a T that's covariant, ACTS on T is contravariant
  // covariance affects how TC instances are fetched

  trait SoundMaker[-T]

  implicit object AnimalSoundMaker extends SoundMaker[Animal]

  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("wow")

  makeSound[Animal] // compiles
  makeSound[Cat] // ok TC instance for animal is also applicable to Cats

  // rule 1: contravariant TCs can use the superclass instance if
  // nothing is available for the given type

  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]

  makeSound[Option[Int]]
  makeSound[Some[Int]]

  // Covariant TC

  trait AnimalShow[+T] {
    def show: String
  }

  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show: String = "all the animals"
  }

  implicit object CatShow extends AnimalShow[Cat] {
    override def show: String = "cats"
  }

  def organizeShow[T](implicit event: AnimalShow[T]): String = event.show

  println(organizeShow[Cat]) // ok injects CatsShow
  // println(organizeShow[Animal]) does nto compile

  // Rule 2: covariant TC will always use the more specific TC instance for that type
  // but maybe confuse the compiler if the general TC is also in scope
  // Cats uses invariant type classes
}
