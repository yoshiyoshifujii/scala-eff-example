package com.github.yoshiyoshifujii.freemonad

object Main extends App {

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  object Functor {
    implicit object OptionFunctor extends Functor[Option] {
      override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
    }

    implicit object ListFunctor extends Functor[List] {
      override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
    }
  }

  sealed trait Free[F[_], A] {
    def map[B](f: A => B)(implicit F: Functor[F]): Free[F, B] = flatMap(a => Pure(f(a)))

    def flatMap[B](f: A => Free[F, B])(implicit F: Functor[F]): Free[F, B] =
      this match {
        case Pure(a) => f(a)
        case Impure(ff) => Impure(F.map(ff)(_.flatMap(f)))
      }
  }

  case class Pure[F[_], A](a: A) extends Free[F, A]

  case class Impure[F[_], A](ff: F[Free[F, A]]) extends Free[F, A]

  type Pair[A] = (A, A)

  implicit val PairFunctor: Functor[Pair] =
    new Functor[Pair] {
      override def map[A, B](fa: (A, A))(f: A => B): (B, B) =
        fa match {
          case (x, y) => (f(x), f(y))
        }
    }

  type Tree[A] = Free[Pair, A]

  def leaf[A](a: A): Tree[A] = Pure(a)

  def node[A](x: Tree[A], y: Tree[A]): Tree[A] = Impure((x, y): Pair[Tree[A]])

  val r: Free[Pair, Int] = for {
    x <- node(leaf(0), node(leaf(1), leaf(2)))
    y <- node(leaf(x), leaf(x))
  } yield y + 1

  assert(r == node(node(leaf(1), leaf(1)), node(node(leaf(2), leaf(2)), node(leaf(3), leaf(3)))))

}
