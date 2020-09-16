package part4typeClasses

import java.util.concurrent.Executors

import cats.{Applicative, Monad}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object HandlingErrors extends App {

  trait MyApplicativeError[M[_], E] extends Applicative[M] {
    def raiseError[A](e: E): M[A]

    def handleErrorWith[A](ma: M[A])(f: E => M[A]): M[A]

    def handleError[A](ma: M[A])(f: E => A): M[A] = handleErrorWith(ma)(e => pure(f(e)))
  }

  trait MyMonadError[M[_], E] extends MyApplicativeError[M, E] with Monad[M] {
    def ensure[A](m: M[A])(error: E)(predicate: A => Boolean): M[A]
  }

  import cats.MonadError
  import cats.instances.either._ // implicit MonadError

  type ErrorOr[A] = Either[String, A]

  val monadErrorEither = MonadError[ErrorOr, String]

  val success = monadErrorEither.pure(22) // Either[String, Int]

  val failure = monadErrorEither.raiseError[Int]("something wrong") // Either[String, Int]

  val handledError = monadErrorEither.handleError(failure)({
    case "bad" => 44
    case _ => 99
  })

  val handleError2 = monadErrorEither.handleErrorWith(failure){
    case "bad" => monadErrorEither.pure(11)
    case _ => monadErrorEither.raiseError[Int]("another bad")
  }

  // filter

  val filteredSuccess = monadErrorEither.ensure(success)("number too small")(_ > 100)

  import cats.instances.try_._

  val exception = new RuntimeException("fail")
  val pureException = MonadError[Try, Throwable].raiseError[Int](exception)

  import cats.instances.future._

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))

  MonadError[Future, Throwable].raiseError(exception) // Future will complete with a Failure(exception)

  // Applicative Error
  import cats.data.Validated
  import cats.instances.list._

  type ErrorsOr[T] = Validated[List[String], T]

  import cats.ApplicativeError

  val applErrorsOr = ApplicativeError[ErrorsOr, List[String]]

  // extension methods

  import cats.syntax.applicative._
  import cats.syntax.applicativeError._

  val extendedSuccess = 42.pure[ErrorsOr]
  val extendedError: ErrorsOr[Int] = List("bad").raiseError[ErrorsOr, Int]
  val recoveredError = extendedError.recover {
    case _ => 42
  }

  import cats.syntax.monadError._

  val testedSuccess = success.ensure("bad")(_ > 100)
}
