package part1intro

import part1intro.Implicits.Person

object TypeClasses extends  App {

  trait JsonSerializer[T] {
    def toJson(value: T): String
  }

  implicit object StringSerializer extends JsonSerializer[String] {
    override def toJson(value: String): String = "\"$value\""
  }

  implicit object IntSerializer extends JsonSerializer[Int] {
    override def toJson(value: Int): String = value.toString
  }

  implicit object PersonSerializer extends JsonSerializer[Person] {
    override def toJson(person: Person): String =
      s"""
         |{"name": "${person.name}"}
       """.stripMargin.trim
  }

  def convertListToJson[T](list: List[T])(implicit serializer: JsonSerializer[T]): String = {
    list.map(serializer.toJson).mkString("[", ", ", "]")
  }

  object JSONSyntax {
    implicit  class JsonSerializable[T](value: T)(implicit  serializer: JsonSerializer[T]) {
      def toJson: String = serializer.toJson(value)
    }
  }

  import JSONSyntax._
  println(convertListToJson(List(Person("Bob"))))
  println(Person("Bob").toJson)

}
