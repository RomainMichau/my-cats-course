package part2abstractMath

object SemiGroups {

  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._

  val naturalIntSemiGroup = Semigroup[Int]
  val intCombination = naturalIntSemiGroup.combine(1, 2)

  import cats.instances.string._

  val stringSemiGroup = Semigroup[String]
  val stringCombin = stringSemiGroup.combine("I love", " cats")

  def reduceInts(list: List[Int]): Int = list.reduce(naturalIntSemiGroup.combine)

  def reduceStrings(list: List[String]): String = list.reduce(stringSemiGroup.combine)

  def reduceGeneral[F](list: List[F])(implicit sg: Semigroup[F]): F = list.reduce(sg.combine)

  case class Expense(id: Long, amount: Double)

  implicit val expSemiGroup = Semigroup.instance[Expense] {
    case (e1, e2) => Expense(e1.id + e2.id, e1.amount + e2.amount)
  }

  def reduceGeneral2[T](list: List[T])(implicit semi: Semigroup[T]): T = list.reduce(_ |+| _)

  def main(args: Array[String]): Unit = {
    println(stringCombin)
    val numbers = (1 to 10).toList
    println(reduceGeneral(numbers))
    val strings = List("I ", "love ", "cats2")
    println(reduceGeneral(strings))
    val numberOption = numbers.map(Option(_))
    val numberOption2 = numbers.map(Option(_)) :+ None :+ Some(100)
    println(reduceGeneral(numberOption))
    println(reduceGeneral(numberOption2))
    val stringOption = strings.map(Option(_))
    val stringOption2 = strings.map(Option(_)) :+ None
    println(stringOption2)
    println(reduceGeneral(stringOption2))
    println(reduceGeneral(stringOption))
    val exps = List(Expense(1, 5.5), Expense(2, 10))
    println(reduceGeneral(exps))
    println(Expense(1, 5.5) |+| Expense(2, 30))
    println(reduceGeneral2(exps))
  }
}
