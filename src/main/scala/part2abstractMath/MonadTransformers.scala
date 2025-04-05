package part2abstractMath

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object MonadTransformers {

  def sumAllOption(values: List[Option[Int]]): Int = ???

  import cats.data.OptionT
  import cats.instances.list._


  val listOfCharOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b')))
  val listOfNbOptions: OptionT[List, Int] = OptionT(List(Option.empty[Int], Option(1), Option(2)))
  val listOfTuples: OptionT[List, (Int, Char)] = for {
    char <- listOfCharOptions
    number <- listOfNbOptions
  } yield (number, char)


  import cats.data.EitherT

  val listOfEithers: EitherT[List, String, Int] = EitherT(List(Right(42), Left("somethingWrong")))
    val futureOfEither: EitherT[Future, String, Int] = EitherT.right(Future(45))

  val bandwith = Map(
    "server1" -> 50,
    "server2" -> 300,
    "server3" -> 170,
  )

  type AsyncResponse[T] = EitherT[Future, String, T]

  def getBandwith(serverName: String): AsyncResponse[Int] = EitherT.apply(Future(bandwith.get(serverName).toRight(s"Not available")))

  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = for {
    s1r <- getBandwith(s1)
    s2r <- getBandwith(s2)
  }yield (s1r + s2r > 250)

  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] = for {
    can <- canWithstandSurge(s1, s2)
    res = if(can) "all good bro" else "oh no not enough ressources bro"
  } yield(res)

  def main(args: Array[String]): Unit = {
    println(listOfTuples.value)
  }
}
