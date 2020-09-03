package part1intro


object Implicits extends App {

  // Implicits

  trait JsonSerializer[T] {
    def toJson(value: T): String
  }

  case class Person(name: String)

  def listToJson[T](list: List[T])(implicit serializer: JsonSerializer[T]): String =
    list.map(serializer.toJson).mkString("[", ", ", "]")

  implicit val presonSerializer: JsonSerializer[Person] = new JsonSerializer[Person] {
    override def toJson(person: Person): String =
      s"""
         |{"name": "${person.name}"}
       """.stripMargin
  }

  val personList = listToJson(List(Person("Alice"), Person("Bob")))

  implicit def oneArgCaseClassSerializer[T <: Product]: JsonSerializer[T] = new JsonSerializer[T] {
    override def toJson(value: T): String =
      s"""
         |"
         |{${value.productElementName(0)}": "${value.productElement(0)}"}
       """.stripMargin
  }

  case class Cat(name: String)

  val catList = listToJson(List(Cat("Fluffy")))

}
