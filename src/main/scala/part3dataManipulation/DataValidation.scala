package part3dataManipulation

object DataValidation {


  import cats.data.Validated
  val aValidValue: Validated[String, Int] = Validated.valid(42)
  val invalidValue: Validated[String, Int] = Validated.invalid("Something went wrong")
  val aTest: Validated[String, Int] = Validated.cond(42 > 39,99, "Nope")

  def isPrimeSimple(num: Int): Boolean = {
    if (num <= 1) false
    else if (num == 2 || num == 3) true
    else
      (2L to num / 2).forall(num % _ != 0)
  }

  // TODO 1
  def validateNumber(n: Int): Validated[List[String], Int] = {
    Validated.cond(n % 2 == 0, n, List("must be even"))
      .combine(Validated.cond(n >= 0 ,n, List("must be non negative")))
      .combine(Validated.cond(n <= 100 ,n, List("too big")))
      .combine(Validated.cond(isPrimeSimple(n) ,n, List("not prime")))
  }

  aValidValue.andThen(v => invalidValue)
  aValidValue.ensure(List("bruh"))(_ == 234)
  aValidValue.map(_ + 1)
  aValidValue.leftMap(_.length)
  aValidValue.bimap(_.length, _ + 1)


  def main(args: Array[String]): Unit = {
    println(invalidValue(543))
  }
}
