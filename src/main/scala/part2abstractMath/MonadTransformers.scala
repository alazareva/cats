package part2abstractMath

import java.util.concurrent.Executors

import scala.concurrent.{ExecutionContext, Future}

object MonadTransformers extends App {

  def sumAppOptions(values: List[Option[Int]]): Int = ???

  import cats.data.OptionT
  import cats.instances.list._

  val listOfNumOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))

  val listOfCharOptions: OptionT[List, Char] = OptionT(List(Option('C'), Option('D')))

  val listOfTuples: OptionT[List, (Int, Char)] = for {
    c <- listOfCharOptions
    n <- listOfNumOptions
  } yield (n, c)

  println(listOfTuples.value)

  // Either transformer

  import cats.data.EitherT
  import cats.instances.future._

  val listOfEither: EitherT[List, String, Int] = EitherT(List(Left("bad"), Right(1)))

  // we need sum of two bandwidths > 250

  val bandwidths = Map(
    "server1" -> 50,
    "server2" -> 300,
    "server3" -> 170,
  )

  type AsyncResponse[T] = EitherT[Future, String, T]

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
    case None => EitherT.left(Future(s"$server unavailable"))
    case Some(b) => EitherT.right(Future(b))
  }

  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = {
    for {
      b1 <- getBandwidth(s1)
      b2 <- getBandwidth(s2)
    } yield b1 + b2 > 250
  }

  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] =
    canWithstandSurge(s1, s2).transform({
      case Left(s) => Left(s"Cannot support spike $s")
      case Right(false) => Left(s"Cannot support not > 250")
      case Right(true) => Right(s"OK")
    })


  generateTrafficSpikeReport("server1", "server2").value.foreach(println)

}
