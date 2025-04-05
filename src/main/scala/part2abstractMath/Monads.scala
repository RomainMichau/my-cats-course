package part2abstractMath

import cats.{Functor, Monoid}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Monads {

  // lists
  val numbersList = List(1, 2, 3)
  val charsList = List('a', 'b', 'c')
  // exo 1.1
  val allCombination = numbersList.flatMap(n => charsList.map(c => (n, c)))
  val allCombinationFor = for {
    n <- numbersList
    c <- charsList
  } yield (n, c)

  // options
  val numOption = Option(2)
  val charOption = Option('a')
  val allCombin = for {
    n <- numOption
    c <- charOption
  } yield (n, c)


  // Futures
  implicit val ec = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val nbFuture = Future(42)
  val charFuture = Future('z')
  val combinFuture = for {
    n <- charFuture
    c <- charFuture
  } yield (n ,c)

  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]
    def flatMap[A, B](e: M[A])(f: A => M[B]): M[B]
    def map[A, B](e: M[A])(f: A => B): M[B] = flatMap(e)(x => pure(f(x)))
  }

  import cats.Monad
  import cats.instances.list._
  import cats.instances.option._
  import cats.instances.future._

  val optionMonad = Monad[Option]
  val anOption = optionMonad.pure(3)
  val aTransformmedOption = optionMonad.flatMap(anOption)(x => if(x%3 == 0) Some(x + 1) else None)

  val listMonad = Monad[List]
  val aList = listMonad.pure(2)
  val aList2 = List(1, 2, 3 , 4)
  val aTransformmedList = listMonad.flatMap(aList2)(x => (1 to x).toList)

  val futureMonad = Monad[Future]
  val aFuture = futureMonad.pure(42)
  val transformedFuture = futureMonad.flatMap(aFuture)(x => Future(x + 1))

  // specialized API
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] = for {
    n <- numbers
    c <- chars
  } yield (n, c)

  // general API
  def getPairListGen[F[_], A, B](numbers: F[A], chars: F[B])(implicit monad: Monad[F]): F[(A, B)] =
    monad.flatMap(numbers)(n => monad.map(chars)(c => (n, c)))

  // extension
  import cats.syntax.applicative._
  val option1 = 1.pure[Option
  ]
  import cats.syntax.flatMap._
  val oneOptionTransformed = option1.flatMap(x => Some(x))








  import cats.syntax.functor._
  def getPairListGen2[F[_]: Monad: Functor, A, B](numbers: F[A], chars: F[B]): F[(A, B)] =
    numbers.flatMap(n => chars.map(c => (n, c)))

  def main(args: Array[String]): Unit = {
  }
}
