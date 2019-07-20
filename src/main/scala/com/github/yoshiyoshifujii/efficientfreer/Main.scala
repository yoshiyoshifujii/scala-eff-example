package com.github.yoshiyoshifujii.efficientfreer


object Main extends App {

  sealed trait Freer[F[_], A] {
    def map[B](f: A => B): Freer[F, B] = flatMap(a => Pure(f(a)))

    def flatMap[B](f: A => Freer[F, B]): Freer[F, B] =
      this match {
        case Pure(a) => f(a)
        case Impure(fa, k) => Impure(fa, k :+ f)
      }
  }

  case class Pure[F[_], A](a: A) extends Freer[F, A]

  case class Impure[F[_], A, B](fa: F[A], k: Arrows[F, A, B]) extends Freer[F, B]

  sealed trait Arrows[F[_], A, B] {
    def :+[C](f: B => Freer[F, C]): Arrows[F, A, C] = Node(this, Leaf(f))

    def ++[C](q: Arrows[F, B, C]): Arrows[F, A, C] = Node(this, q)

    def view: View[F, A, B] =
      this match {
        case Leaf(f) => One(f)
        case Node(l, r) =>
          @scala.annotation.tailrec
          def go[T](x: Arrows[F, A, T], y: Arrows[F, T, B]): View[F, A, B] =
            x match {
              case Leaf(f) => Cons(f, y)
              case Node(l, r) => go(l, Node(r, y))
            }
          go(l, r)
      }

    def apply(a: A): Freer[F, B] = {
      @scala.annotation.tailrec
      def go[G](arrows: Arrows[F, G, B], a: G): Freer[F, B] =
        arrows.view match {
          case One(f) => f(a)
          case Cons(f, r) =>
            f(a) match {
              case Pure(v) => go(r, v)
              case Impure(fa, l) => Impure(fa, l ++ r)
            }
        }
      go(this, a)
    }
  }

  case class Leaf[F[_], A, B](f: A => Freer[F, B]) extends Arrows[F, A, B]

  case class Node[F[_], A, B, C](left: Arrows[F, A, B], right: Arrows[F, B, C]) extends Arrows[F, A, C]

  object Freer {
    def apply[F[_], A](fa: F[Freer[F, A]]): Freer[F, A] = Impure(fa, Leaf((a: Freer[F, A]) => a))
  }

  sealed trait View[F[_], A, B]

  case class One[F[_], A, B](f: A => Freer[F, B]) extends View[F, A, B]

  case class Cons[F[_], A, B, C](f: A => Freer[F, B], k: Arrows[F, B, C]) extends View[F, A, C]
}
