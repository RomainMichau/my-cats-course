package part3dataManipulation

object Readers {

  case class Config(dbUsername: String, dbPassword: String, host: String, port: Int, nThread: Int, emailReplyTo: String)

  case class DbConnection(username: String, password: String) {
    def getOrderStatus(orderId: Long): String = "dispatched"
    def lastOrderId(username: String): Long = 123
  }

  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("server started")
  }

  val config = Config("theUsername", "thePassword", "0.0.0.0", 8080, 8, "noreply@mail.com")

  import cats.data.Reader
  val dbReader: Reader[Config, DbConnection] = Reader(c => DbConnection(c.dbUsername, c.dbPassword))

  val dbConn = dbReader.run(config)
  val danierOrderIdReader: Reader[Config, String] = dbReader.map(_.getOrderStatus(55))
  val danierOrderStatus = danierOrderIdReader.run

  def getLastOrderStatus(username: String): String = {
    val userLastOrderIdReader = dbReader.map(_.lastOrderId(username))
      .flatMap(lastOrderId => dbReader.map(_.getOrderStatus(lastOrderId)))
    userLastOrderIdReader.run(config)

    val usersOrderFor = for {
      lastOrderId <- dbReader.map(_.lastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
    } yield orderStatus
    usersOrderFor.run(config)
  }

  // EXO
  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, content: String) = s"$emailReplyTo Sending Email to $address with $content"
  }


  val emailReader = Reader[Config, EmailService](c => EmailService(c.emailReplyTo))
  def emailUser(username: String, userEmail: String) = {
    val reader = for {
      lastOrderId <- dbReader.map(_.lastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
      mail <- emailReader.map(_.sendEmail(userEmail, s"orderStatus status: $orderStatus"))
    } yield (mail)
    reader.run(config)
  }



  def main(args: Array[String]): Unit = {
    print(emailUser("romain", "romain9692@gmail.com"))
  }
}
