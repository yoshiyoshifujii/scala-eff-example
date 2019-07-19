package com.github.yoshiyoshifujii.freermonad

object Main extends App {

  case class Coyoneda[F[_], A, B](fa: F[A], k: A => B) {
    def map[C](f: B => C): Coyoneda[F, A, C] =
      Coyoneda(fa, k andThen f)
  }

  sealed trait Freer[F[_], A] {

    def map[B](f: A => B): Freer[F, B] = flatMap(a => Pure(f(a)))

    def flatMap[B](f: A => Freer[F, B]): Freer[F, B] =
      this match {
        case Pure(a) => f(a)
//        case Impure(fa, k) => Impure(fa, (a: Any) => k(a) flatMap f)
        case Impure(c) => Impure(c.map(_.flatMap(f(_))))
      }

  }

  case class Pure[F[_], A](a: A) extends Freer[F, A]

//  case class Impure[F[_], A, B](fa: F[A], k: A => Freer[F, B]) extends Freer[F, B]
  case class Impure[F[_], A, B](c: Coyoneda[F, A, Freer[F, B]]) extends Freer[F, B]

  object Freer {
    def apply[F[_], A](ff: F[Freer[F, A]]): Freer[F, A] =
      Impure(Coyoneda(ff, (x: Freer[F, A]) => x))
  }

  type ConstUnit[A] = Unit

  type Maybe[A] = Freer[ConstUnit, A]

  def some[A](a: A): Maybe[A] = Pure(a)

  def none[A]: Maybe[A] = Freer((): ConstUnit[Maybe[A]])

  def safeDiv(n: Int, d: Int): Maybe[Int] = if (d == 0) none else some(n / d)

  val r = for {
    n <- safeDiv(4, 2)
    m <- safeDiv(n, 0)
  } yield m

  def maybe[A](m: Maybe[A])(default: A): A = m match {
    case Pure(a) => a
    case Impure(Coyoneda((), _)) => default
  }

  assert(maybe(r)(42) == 42)

}
