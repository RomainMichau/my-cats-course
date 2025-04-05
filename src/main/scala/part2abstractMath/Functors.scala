package part2abstractMath

import scala.util.Try

object Functors {

  import cats.Functor
  import cats.syntax.functor._
  import cats.instances.list._

  val aModifiedList = List(1, 2, 3).map(_ + 1) // List 1 2 3 4
  val aModifiedTry = Try(42).map(_ + 1)

  trait MyFunctor[F[_]] {
    def map[A, B](value: F[A])(func: A => B): F[B]
  }

  val listFunctor = Functor[List]
  val incrementedNumber = listFunctor.map(List(1, 2, 3))(_ + 1)

  val optionFunctor = Functor[Option]
  val incrementedOption = optionFunctor.map(Some(1))(_ + 1)

  val anIncrementedTry = Functor[Try].map(Try(32))(_ + 1)

  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)

  def do10x[F[_]](elem: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(elem)(_ * 10)

  // exo 1
  trait Tree[+T]
  object Tree {
    def leaf[T](v: T): Tree[T] = Leaf(v)
    def branch[T](v: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(v, left, right)
  }
  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]
  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = {
      fa match {
        case Leaf(v) => Leaf(f(v))
        case Branch(v, left, right) => Branch(f(v), map(left)(f), map(right)(f))
      }
    }
  }
  val myTree = Tree.branch(32, Tree.leaf(10), Tree.branch(14, Tree.leaf(12), Tree.leaf(54)))

  import cats.syntax.functor._
  val myTree2 = myTree.map(_ * 100)

  def do10xShorter[F[_]: Functor](elem: F[Int]): F[Int] = elem.map(_ * 10)


  def main(args: Array[String]): Unit = {
    println(do10x(List(1, 2, 3)))
    println(do10x(Option(10)))
    println(do10xShorter(Try(10)))
    println(do10x[Tree](myTree))
    println(do10xShorter[Tree](myTree))
    println(myTree2)
  }
}
