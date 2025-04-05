package part2abstractMath

import scala.annotation.tailrec

object CustomMonads {

  import cats.Monad
  import cats.syntax._

  implicit object OptionMonad extends Monad[Option] {
    override def pure[A](x: A): Option[A] = Some(x)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
      case Some(a) => f(a)
      case None => None
    }

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
      case None => None
      case Some(Left(value)) => tailRecM(value)(f)
      case Some(Right(value)) => Some(value)
    }
  }

  type Identity[T] = T
  val aNumber: Identity[Int] = 42

  object IdentityMonad extends Monad[Identity] {
    override def pure[A](x: A): Identity[A] = x

    override def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = f(a) match {
      case Left(v) => tailRecM(v)(f)
      case Right(v) => v
    }
  }

  sealed trait Tree[+A]

  case class Leaf[+A](value: A) extends Tree[A]

  case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object TreeMonad extends Monad[Tree] {
    override def pure[A](x: A): Tree[A] = Leaf(x)

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(value) => f(value)
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
    }


    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {

      def stackRef(t: Tree[Either[A, B]]): Tree[B] = t match {
        case Leaf(Left(v)) => stackRef(f(v))
        case Leaf(Right(v)) => Leaf(v)
        case Branch(l, r) => Branch(stackRef(l), stackRef(r))
      }


      stackRef(f(a))
    }
  }


  def main(args: Array[String]): Unit = {

  }
}
