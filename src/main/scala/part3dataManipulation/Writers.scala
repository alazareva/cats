package part3dataManipulation

import java.util.concurrent.Executors

import scala.concurrent.{ExecutionContext, Future}

object Writers extends App {

  import cats.data.Writer
  import cats.instances.vector._
  import cats.instances.list._

  val aWriter: Writer[List[String], Int] = Writer(List("Started"), 45)
  val anIncreasedWriter = aWriter.map(_ + 1)
  val aLogsWriter = aWriter.mapWritten(_:+ "Found something interesting")
  val aWriterWithBoth = aWriter.bimap(_:+ "Found something interesting", _ + 1)
  val aWriterWithBoth2 = aWriter.mapBoth((logs, value) => (logs :+ s"Found something interesting $value", value + 1))

  val desiredValue = aWriter.value
  val logs = aWriter.written

  val (l, v) = aWriter.run

  val writerA = Writer(Vector("log a1", "log a2"), 20)
  val writerB = Writer(Vector("log b1", "log b2"), 30)

  val compositeWriter = for {
    va <- writerA
    vb <- writerB
  } yield va + vb

  println(compositeWriter.run)

  // to reset logs

  val anEmptyWriter = aWriter.reset

  // Ex rewrite a function that prints something with writers

  def countAndSay(n: Int): Unit = {
  if (n <= 0) println("Starting")
  else {
    countAndSay(n - 1)
    println(n)
  }
  }

  def countAndLog(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector("Starting"), n)
    else countAndLog(n - 1).flatMap(_ => Writer(Vector(s"$n"), n))
  }

  //countAndLog(5).written.foreach(println)
  //countAndSay(4)

  // Ex

  def naiveSum(n: Int): Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Captured at $lowerSum")
      lowerSum + n
    }
  }

  def sumWithWriters(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector.empty, 0)
    else for {
      _ <- Writer(Vector(s"Now at $n"), n)
      lowerSum <- sumWithWriters(n - 1)
      _ <- Writer(Vector(s"Captured at $lowerSum"), n)
    } yield lowerSum + n
  }

  def sumWithWriters2(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector.empty, 0)
    else sumWithWriters2(n - 1).mapBoth( (logs, lowerSum) => {
      (s"Now at $n" +: logs :+ s"Captured at $lowerSum", lowerSum + n)
    })
  }

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  println(naiveSum(5))
  sumWithWriters2(5).written.foreach(println)
  val writerF1 = Future(sumWithWriters2(5))
  val writerF2 = Future(sumWithWriters2(5))

  val logs1 = writerF1.map(_.written)
  val logs2 = writerF2.map(_.written)

}
