package part2abstractMath

import cats.implicits.catsSyntaxApplicativeId

object UsingMonads {

  import cats.Monad
  import cats.instances.list._
  import cats.syntax.functor._
  import cats.syntax.flatMap._

  val monadList = Monad[List]
  val aSimpleList = 2.pure

  val aManualEither: Either[String, Int] = Right(42)
  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]

  import cats.instances.either._

  val loadingMonad = Monad[LoadingOr]
  val anEither = loadingMonad.pure(42)
  val aChangedEither = loadingMonad.flatMap(anEither)(n => if (n % 2 == 0) Right(n + 1) else Left("bro"))

  case class OrderStatus(orderId: Long, status: String)

  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] = Right(OrderStatus(orderId, "ready to ship"))

  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.orderId > 1000) Left("not available yet") else Right("Amsterdam, NL")

  val orderId = 457L
  val orderLocation = getOrderStatus(orderId).flatMap(trackLocation)

  case class Connection(host: String, port: String)

  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]

    def issueRequest(connection: Connection, payload: String): M[String]
  }

  class MyHttpService extends HttpService[Option] {
    override def getConnection(cfg: Map[String, String]): Option[Connection] = cfg.get("host")
      .flatMap(host => cfg.get("port").map((host, _)).map { case (host, port) => Connection(host, port) })

    override def issueRequest(connection: Connection, payload: String): Option[String] = if (payload.length < 40) {
      Some("yes bro")
    }
    else None
  }

  object AggressiveHttpService extends HttpService[ErrorOr] {
    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] = (for {
      port <- cfg.get("port")
      host <- cfg.get("host")
    } yield Connection(host, port)).toRight(new Exception("brooo"))

    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] = if (payload.length < 40) {
      Right("yes bro")
    }
    else Left(new Exception("broo"))
  }

  def main(args: Array[String]): Unit = {

  }
}
