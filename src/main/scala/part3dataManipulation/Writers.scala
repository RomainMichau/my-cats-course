package part3dataManipulation

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

object Writers {


  import cats.data.Writer

  val aWriter: Writer[List[String], Int] = Writer(List("Started"), 45)
  val anIncreasedWriter = aWriter.map(_ + 1)
  val aLogsWriter = aWriter.mapWritten(_ :+ "found something intersting")
  val aWriterWithBoth = aWriter.bimap(_ :+ "found something intersting", _ + 1)
  val aWriterWithBoth2 = aWriter.mapBoth { case (list, value) => (list :+ "yep bro", value + 1) }


  val writterA = Writer(Vector("LogA1", "LogA2"), 10)
  val writterB = Writer(Vector("LogB1", "LogB2"), 10)

  val compositeWriter = for {
    va <- writterA
    vb <- writterB
  } yield va + vb

  // reset the log
  val anEmptyWriter = aWriter.reset

  val desiredValue = aWriter.value
  val logs = aWriter.written
  val (l, v) = aWriter.run

  // exo
  def countAndLog(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) {
      Writer(Vector("starting"), n)
    }
    else {
      countAndLog(n - 1).flatMap(_ => Writer(Vector(s"$n"), n))
    }
  }

  def naiveSum(n: Int): Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computer sum(${n - 1}) = $lowerSum")
      lowerSum + n
    }
  }

  def naiveSumWriter(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer[Vector[String], Int](Vector(), 0)
    else for {
      _ <- Writer(Vector(s"Now at $n"), n)
      lowerSum <- naiveSumWriter(n - 1)
      _ <- Writer(Vector(s"Computer sum(${n - 1}) = $lowerSum"), n)
    } yield n + lowerSum
  }

  def main(args: Array[String]): Unit = {
//    Future(naiveSum(100)).foreach(println)
//    Future(naiveSum(100)).foreach(println)
    val sumFuture1 = Future(naiveSumWriter(100))
    val sumFuture2 = Future(naiveSumWriter(100))
    val logs1 = sumFuture1.map(_.written).map(println)
    val logs2 = sumFuture2.map(_.written).map(println)
    Await.result(logs1, 100.seconds)
    Await.result(logs2, 100.seconds)
  }
}
