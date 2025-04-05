package part3dataManipulation

object Evaluations {


  import cats.Eval
  val instantEval: Eval[Int] = Eval.now {
    println("computing now")
    123
  }
  val redoEval: Eval[Int] = Eval.always{
    println("Computing again")
    456
  }

  val delayedEval: Eval[Int] = Eval.later {
    println("Computing later")
    789
  }
  val composedEvaluation = instantEval.flatMap(v1 => delayedEval.map(v2 => v1 + v2))
  val composedEvaluation2 = for {
    v1 <- instantEval
    v2 <- delayedEval
  } yield v1 + v2

  // TODO 1
  val evalExo1 = for {
    a <- delayedEval
    b <- redoEval
    c <- instantEval
    d <- redoEval
  } yield a + b + c + d

  def main(args: Array[String]): Unit = {
    println(evalExo1.value)
//    println(composedEvaluation.value)
  }
}
