package com.github.yoshiyoshifujii.effmonad

object Main extends App {

  sealed trait Union[F[_], G[_], A]

  case class Inl[F[_], G[_], A](value: F[A]) extends Union[F, G, A]

  case class Inr[F[_], G[_], A](value: G[A]) extends Union[F, G, A]

  sealed trait Void[A]

  type :+:[F[_], G[_]] = { type R[A] = Union[F, G, A] }

  val u1: (List :+: (Option :+: Void)#R)#R[Int] = Inr(Inl(Some(0)))

  trait Member[F[_], G[_]] {
    def inject[A](fa: F[A]): G[A]
  }

  object Member {
    implicit def left[F[_], G[_]]: Member[F, (F :+: G)#R] =
      new Member[F, (F :+: G)#R] {
        override def inject[A](fa: F[A]): Union[F, G, A] = Inl(fa)
      }

    implicit def right[F[_], G[_], H[_]](implicit member: Member[F, H]): Member[F, (G :+: H)#R] =
      new Member[F, (G :+: H)#R] {
        override def inject[A](fa: F[A]): Union[G, H, A] = Inr(member.inject(fa))
      }
  }

  val u2 = implicitly[Member[Option, (List :+: (Option :+: Void)#R)#R]].inject(Some(0))

  sealed trait Eff[R[_], A] {
    def map[B](f: A => B): Eff[R, B] = flatMap(a => Pure(f(a)))

    def flatMap[B](f: A => Eff[R, B]): Eff[R, B] =
      this match {
        case Pure(a) => f(a)
        case Impure(r, k) => Impure(r, k :+ f)
      }
  }

  case class Pure[R[_], A](a: A) extends Eff[R, A]

  case class Impure[R[_], A, B](union: R[A], k: Arrows[R, A, B]) extends Eff[R, B]

  sealed trait Arrows[F[_], A, B] {
    def :+[C](f: B => Eff[F, C]): Arrows[F, A, C] = Node(this, Leaf(f))

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

    def apply(a: A): Eff[F, B] = {
      @scala.annotation.tailrec
      def go[G](arrows: Arrows[F, G, B], a: G): Eff[F, B] =
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

  case class Leaf[F[_], A, B](f: A => Eff[F, B]) extends Arrows[F, A, B]

  case class Node[F[_], A, B, C](left: Arrows[F, A, B], right: Arrows[F, B, C]) extends Arrows[F, A, C]

  sealed trait View[F[_], A, B]

  case class One[F[_], A, B](f: A => Eff[F, B]) extends View[F, A, B]

  case class Cons[F[_], A, B, C](f: A => Eff[F, B], k: Arrows[F, B, C]) extends View[F, A, C]

  object Eff {
    def apply[R[_], F[_], A](fa: F[A])(implicit F: Member[F, R]): Eff[R, A] =
      Impure(F.inject(fa), Leaf((x: A) => Pure(x)))

    def run[A](eff: Eff[Void, A]): A =
      eff match {
        case Pure(a) => a
      }
  }

  sealed trait Writer[+A]

  case class Tell(value: String) extends Writer[Unit]

  def tell[R[_]](value: String)(implicit w: Member[Writer, R]): Eff[R, Unit] = Eff(Tell(value))

  object Writer {
    def run[R[_], A](eff: Eff[(Writer :+: R)#R, A]): Eff[R, (String, A)] =
      eff match {
        case Pure(a) => Pure(("", a))
        case Impure(Inl(Tell(v)), k) => run(k(())).map { case (s, a) => (v + s, a) }
        case Impure(Inr(r), k) => Impure(r, Leaf((a: Any) => run(k(a))))
      }
  }

  def e1[R[_]](implicit w: Member[Writer, R]) =
    for {
      _ <- tell("hello, ")
      _ <- tell("world.")
    } yield 0

  assert(Eff.run(Writer.run(e1)) == ("hello, world.", 0))

  case class Maybe[A]()

  def some[R[_], A](a: A): Eff[R, A] = Pure(a)
  def none[R[_], A](implicit m: Member[Maybe, R]): Eff[R, A] = Eff(Maybe[A]())

  object Maybe {
    def run[R[_], A](eff: Eff[(Maybe :+: R)#R, A])(default: A): Eff[R, A] =
      eff match {
        case Pure(a) => Pure(a)
        case Impure(Inl(Maybe()), _)  => Pure(default)
        case Impure(Inr(r), k) => Impure(r, Leaf((a: Any) => run(k(a))(default)))
      }
  }

  def e2[R[_]](implicit m: Member[Maybe, R]): Eff[R, Int] =
    for {
      x <- some(2)
      y <- none[R, Int]
    } yield x + y

  assert(Eff.run(Maybe.run[Void, Int](e2)(-1)) == -1)

//  def e3[R[_]](implicit w: Member[Writer, R], m: Member[Maybe, R]) =
//    for {
//      _ <- tell("hello, ")
//      _ <- none[R, Unit]
//      _ <- tell("world.")
//    } yield 0
//
//  assert(Eff.run(Writer.run(Maybe.run(e3)(-1))) == ("hello, ", -1))

}
