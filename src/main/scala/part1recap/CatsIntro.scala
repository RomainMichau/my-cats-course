package part1recap

object CatsIntro {
  val aComparison = 2 == "a string"

  import cats.Eq
//  import cats.instances.int
//  import cats.instances.list
  import cats.syntax.eq._

  val intEquality = Eq[Int]

  val typeSafeComp = intEquality.eqv(2, 3)
  //  val anUnsafeComp = intEquality.eqv(2, "yo")

  2 === 3
  val neqComp = 2 =!= 2
  val aListComparison = List(2) === List(3)

  case class ToyCar(model: String, price: Double)
  implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar]{(c1, c2) =>
    c1.price == c2.price
  }


  def main(args: Array[String]): Unit = {
    println("o")
  }
}
