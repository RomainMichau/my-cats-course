package part3dataManipulation

import cats.Eval

object FunctionalState {

//  type MyState[S, A] = S => (S, A)

  import cats.data.State

  val countAndSay: State[Int, String] = State(currentCount => (currentCount + 1, s"Counted $currentCount"))
  val (eleven, counted10) = countAndSay.run(10).value
  // state = "iterative" computations

  // iterative
  var a = 10
  a += 1
  val firstComputation = s"Added 1 to 10, obtained $a"
  a *= 5
  val secondComputation = s"Multiplied with 5, obtained $a"

  // pure FP with states
  val firstTransformation: State[Int, String] = State((s: Int) => (s + 1, s"Added 1 to 10, obtained ${s + 1}"))
  val secondTransformation = State((s: Int) => (s * 5, s"Multiplied with 5, obtained ${s * 5}"))
  val compositeTransformation: State[Int, (String, String)] = firstTransformation.flatMap { firstResult =>
    secondTransformation.map(secondResult => (firstResult, secondResult))
  }
  val compositeTransformation2 = for {
    firstResult <- firstTransformation
    secondResult <- secondTransformation
  } yield (firstResult, secondResult)

  // function composition is clunky
  val func1 = (s: Int) => (s + 1, s"Added 1 to 10, obtained ${s + 1}")
  val func2 = (s: Int) => (s * 5, s"Multiplied with 5, obtained ${s * 5}")
  val compositeFunc = func1.andThen {
    case (newState, firstResult) => (firstResult, func2(newState))
  }

  // TODO 1
  object ShoppingCart {
    def empty() = ShoppingCart(List.empty, 0.0)
  }
  case class ShoppingCart(items: List[String], total: Double)
  def addToCart(item: String, price: Double): State[ShoppingCart, Double] =
    State{case ShoppingCart(oitem, oprice) =>(ShoppingCart(item +: oitem, oprice + price), oprice + price)}

  def addToCart2(item: String, price: Double): MyState[ShoppingCart, Double] =
    MyState{case ShoppingCart(oitem, oprice) =>(ShoppingCart(item +: oitem, oprice + price), oprice + price)}

//  addToCart("guitar", 150.0).flatMap(s => )
  val danielCart : State[ShoppingCart, Double]= for {
    _ <- addToCart("guitar", 150.0)
    _ <- addToCart("armonica", 30.0)
    total <- addToCart("beer", 15.0)
  } yield total

  val danielCart2 : MyState[ShoppingCart, Double]= for {
    _ <- addToCart2("guitar", 150.0)
    _ <- addToCart2("armonica", 30.0)
    total <- addToCart2("beer", 15.0)
  } yield total



  // TODO 2
  def inspect[A, B](f: A => B): State[A, B] = State[A, B](state => (state, f(state)))
  def get[A]: State[A, A] = State(state => (state, state))
  def set[A](value: A): State[A, Unit] = State(_ => (value, ()))
  def modify[A](f: A => A): State[A, Unit] = State(state => (f(state), ()))

  import cats.data.State._
  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a+ 10)
    b <- get[Int]
    _ <- modify[Int](_ +4)
    c <- inspect[Int, Int](_ * 2)
  } yield (a ,b ,c)

  val program2: MyState[Int, (Int, Int, Int)] = for {
    a <- MyState.get[Int]
    _ <- MyState.set[Int](a+ 10)
    b <- MyState.get[Int]
    _ <- MyState.modify[Int](_ +4)
    c <- MyState.inspect[Int, Int](_ * 2)
  } yield (a ,b ,c)

  def main(args: Array[String]): Unit = {
//    assert(danielCart.run(ShoppingCart.empty()).value._2 == 195.0)
//    assert(danielCart2.run(ShoppingCart.empty())._2 == 195.0)
//    println(danielCart2.run(ShoppingCart.empty())._2 )
    println(program.run(0).value)
    println(program2.run(0))
  }

  object MyState {
    def apply[S, A](f : S => (S, A)): MyState[S, A] = new MyState[S, A](f)
    def inspect[A, B](f: A => B): MyState[A, B] = MyState[A, B](state => (state, f(state)))
    def get[A]: MyState[A, A] = MyState(state => (state, state))
    def set[A](value: A): MyState[A, Unit] = MyState(_ => (value, ()))
    def modify[A](f: A => A): MyState[A, Unit] = MyState(state => (f(state), ()))
  }

  class MyState[S, A](val run : S => (S, A)) {
    def flatMap[A2](f: A => MyState[S, A2]): MyState[S, A2] = {
      MyState{bro =>
        val (s, a) = run(bro)
        f(a).run(s)
      }
    }
    def map[A2](f: A => A2): MyState[S, A2] = {
      MyState {bro =>
        val (s, a) = run(bro)
        (s, f(a))
      }
    }
  }
}