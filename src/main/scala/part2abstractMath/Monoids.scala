package part2abstractMath


object Monoids {
  import cats.Monoid
  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.monoid._
  import cats.instances.option._
  val numbers = (1 to 100).toList
  val strings = List("I", "love", "cats")
  val sumLeftInt = numbers.foldLeft(0)(_ |+| _)
  val sumRightInt = numbers.foldRight(0)(_ |+| _)

  val sumLeftString = strings.foldLeft("")(_ |+| _)
  val sumRightString = strings.foldRight("")(_ |+| _)
  val intMonoid = Monoid[Int]



  val emptyOption = Monoid[Option[Int]].empty
  val combineOption = Monoid[Option[Int]].combine(Option(2), Option.empty[Int]) // Some 2
  val combineOption2 = Monoid[Option[Int]].combine(Option(3), Option(6)) // Some 2


  // Exo1
  def combineFold[T](list: List[T])(implicit monoid: Monoid[T]): T =
    list.foldLeft(monoid.empty)(_ |+| _)

  // Exo2
  val phoneBooks = List(
    Map(
      "Alice" -> 235,
      "Bob" -> 236
    ),
    Map(
      "Charlie" -> 123,
      "Danier" -> 901
    ),
    Map(
      "Tina" -> 547,
    )
  )

  // Exo 3
  implicit val shopMonoid = Monoid.instance[ShoppingCart](
    ShoppingCart(List.empty, 0),
    {
      case (s1, s2) =>
        ShoppingCart(s1.items ++ s2.items, s1.total + s2.total)
    }
  )
  case class ShoppingCart(items: List[String], total: Double)
  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart = combineFold(shoppingCarts)


  def main(args: Array[String]): Unit = {
    println(sumRightString)
    println(sumLeftString)
    println("I" |+| "love")
    println("love" |+| "I")
    println(combineFold(phoneBooks))
  }
}
