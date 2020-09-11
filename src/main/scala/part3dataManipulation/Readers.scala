package part3dataManipulation

object Readers extends App {

  /*
  - application file => initial data structure
  - a DB layer
  - HTTP layer
  - business logic layer
   */

  case class Configuration(dbUsername: String, dbPassword: String, host: String, port: Int, nThreads: Int, emailReply: String)

  case class DbConnection(username: String, password: String) {
    def getOrderStatus(oId: Long): String = "dispatched"
    def getLastOrderId(user: String): Long = 1234L
  }

  case class HTTPService(host: String, port: Int) {
    def start(): Unit = println("server started")
  }
  case class EmailService(replyTo: String) {
    def sendEmail(address: String, constants: String): String = s"From $replyTo to: $address >>> $constants"
  }


  val config = Configuration("dbuesr", "dbpw", "localhost", 1223, 8, "foo@gmail.com")

  import cats.data.Reader

  val dbReader: Reader[Configuration, DbConnection] = Reader(conf => DbConnection(conf.dbUsername, conf.dbPassword))
  val emailReader: Reader[Configuration, EmailService] = Reader(conf => EmailService(conf.emailReply))
  val dbConn = dbReader.run(config)
  val orderStatusReader: Reader[Configuration, String] = dbReader.map(dbConn => dbConn.getOrderStatus(42))
  val orderStatus: String = orderStatusReader.run(config)

  def getLastOrderStatus(user: String): String =  {
    val reader = for {
      lastOrder <- dbReader.map(_.getLastOrderId(user))
      status <- dbReader.map(_.getOrderStatus(lastOrder))
    } yield status
    reader.run(config)
  }
  println(getLastOrderStatus("x"))

  // Ex

  def emailUser(username: String, userEmail: String) = {
    val reader = for {
      lastOrder <- dbReader.map(_.getLastOrderId(username))
      status <- dbReader.map(_.getOrderStatus(lastOrder))
      email <- emailReader.map(_.sendEmail(userEmail, s"your order is $status"))
    } yield email
    reader.run(config)
  }

  println(emailUser("Ana", "bar@gmail.com"))

}
