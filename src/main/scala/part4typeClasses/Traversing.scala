package part4typeClasses

import java.util.concurrent.Executors

import cats.{Applicative, Foldable, Functor, Monad, data}

import scala.concurrent.{ExecutionContext, Future}

object Traversing extends App {

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))

  val servers: List[String] = List("server-ci.foo.com", "server-prod.foo.com", "server-staging.foo.com")

  def getBandwidth(host: String): Future[Int] = Future(host.length)

  val allBandwidths: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int])) { (acc, host) =>
    val bandFuture = getBandwidth(host)
    for {
      a <- acc
      band <- bandFuture
    } yield a :+ band
  }

  val allBandwidthsTraverse: Future[List[Int]] = Future.traverse(servers)(getBandwidth)

  val allBandwidthsSequence: Future[List[Int]] = Future.sequence(servers.map(getBandwidth))

  // Ex

  import cats.syntax.applicative._
  import cats.syntax.apply._

  def listTraverse[F[_] : Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] = {
    list.foldLeft(List.empty[B].pure[F]) { (accf, a) =>
      (accf, func(a)).mapN(_ :+ _)
    }
  }

  // Ex
  def listSequence[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] =
    listTraverse(list)(identity)

  import cats.instances.option._
  def filerAsOption(list: List[Int])(predicate: Int => Boolean): Option[List[Int]] =
    listTraverse[Option, Int, Int](list)(n => Some(n).filter(predicate))

  val filteredOpt = filerAsOption(List(1, 2, 3))(_ % 2 == 0) // None

  import cats.data.Validated
  import cats.instances.list._

  type ErrorOr[T] = Validated[List[String], T]

  def filterAsValidated(list: List[Int])(predicate: Int => Boolean): ErrorOr[List[Int]] =
    listTraverse[ErrorOr, Int, Int](list){ n =>
      if (predicate(n)) Validated.valid(n)
      else Validated.invalid(List(s"predicate failed for $n"))
    }

  val filteredValidated = filterAsValidated(List(1, 2, 3))(_ % 2 == 0 ) // Invalid(...for 1, ...for 2)

  println(filteredValidated)

  import cats.Id

  trait MyTraverse[L[_]] extends Foldable[L] with Functor[L] {
    def traverse[F[_] : Applicative, A, B](container: L[A])(func: A => F[B]): F[L[B]]
    def sequence[F[_]: Applicative, A](container: L[F[A]]): F[L[A]] = traverse(container)(identity)

    def map[A, B](wa: L[A])(f: A => B): L[B] = {
      traverse[Id, A, B](wa)(a => f(a))
    }
  }

  import cats.Traverse
  import cats.instances.future._

  val allBandwidthsCats = Traverse[List].traverse(servers)(getBandwidth)

  import cats.syntax.traverse._

  val allBandwidthsCats2 = servers.traverse(getBandwidth)

  // Ex
}
