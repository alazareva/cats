package part2abstractMath

object UsingMonads extends App {

  import cats.Monad
  import cats.instances.list._

  val monadList = Monad[List]

  val aSimpleList = monadList.pure(1)
  val extendList = monadList.flatMap(aSimpleList)(x => monadList.pure(x + 1))

  val aManualEither: Either[String, Int] = Right(42)

  // Either is also a monad

  import cats.instances.either._

  type LoadingOr[T] = Either[String, T]

  val loadingMonad = Monad[LoadingOr]

  val anEither = loadingMonad.pure(34)
  val aChangedLoading = loadingMonad.flatMap(anEither)(
    n => if (n % 2 == 0) Right(n + 1) else Left("loading"))

  case class OrderStatus(oid: Long, status: String)

  def getOrderStatus(oid: Long): LoadingOr[OrderStatus] = Right(OrderStatus(oid, "ready to ship"))

  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.oid > 1000) Left("Not available yet") else Right("Amsterdam")

  val oid = 123L
  val orderLocation = loadingMonad.flatMap(getOrderStatus(oid))(trackLocation)

  // using extension methods

  import cats.syntax.flatMap._
  import cats.syntax.functor._

  val orderLocationBetter = getOrderStatus(oid).flatMap(trackLocation)

  val orderLocationFor = for {
    os <- getOrderStatus(oid)
    loc <- trackLocation(os)
  } yield loc

  // Ex

  case class Connection(host: String, port: String)
  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String] // if payload > 20 chars fail
  }

  // provide a real implementation of the trait using one of the Monads


  object OptionHttpService extends HttpService[Option] {
    override def getConnection(cfg: Map[String, String]): Option[Connection] = for {
      host <- cfg.get("host")
      port <- cfg.get("port")
    } yield Connection(host, port)

    override def issueRequest(connection: Connection, payload: String): Option[String] =
      if (payload.length < 20) Some("response") else None
  }

  val responseOption = for {
    conn <- OptionHttpService.getConnection(config)
    resp <- OptionHttpService.issueRequest(conn, "req")
  } yield resp

  object LoadingOrHttpService extends HttpService[LoadingOr] {
    override def getConnection(cfg: Map[String, String]): LoadingOr[Connection] = {
      val conn = for {
        host <- cfg.get("host")
        port <- cfg.get("port")
      } yield Connection(host, port)

      conn match {
        case None => Left("bad config")
        case Some(c) => Right(c)
      }
    }

    override def issueRequest(connection: Connection, payload: String): LoadingOr[String] = {
      if (payload.length > 20) Left("too long") else Right("response")
    }
  }

  println(responseOption)

  val responseEither= for {
    conn <- LoadingOrHttpService.getConnection(config)
    resp <- LoadingOrHttpService.issueRequest(conn, "req")
  } yield resp


  def getResponse[M[_]: Monad](service: HttpService[M], payload: String): M[String] =
    for {
      conn <- service.getConnection(config)
      resp <- service.issueRequest(conn, payload)
    } yield resp

  println(responseEither)

  import cats.instances.option._

  println(getResponse(OptionHttpService, "hi"))

}
