package exercises

import scala.annotation.tailrec

/**
  * FP in Scala : Chapter 5
  *
  * @author lepnio
  * @project fp-scala
  */
object Exercises5 {

  // streams

  sealed trait Stream[+A] {

    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
    }

    // exercise 5.1

    def toList: List[A] = this match {
      case Empty => Nil
      case Cons(h, tl) => h() :: tl().toList
    }

    def toListRec: List[A] = {
      @tailrec
      def go(s: Stream[A], acc: List[A]): List[A] = s match {
        case Empty => acc
        case Cons(h, tl) => go(tl(), h() :: acc)
      }

      go(this, Nil).reverse
    }

    // exercise 5.2

    def take(n: Int): Stream[A] = this match {
      case Empty => Empty
      case Cons(h, tl) => if (n == 0) Empty else Cons(h, () => tl().take(n - 1))
    }

    @tailrec
    final def drop(n: Int): Stream[A] = this match {
      case Empty => Empty
      case Cons(h, tl) => if (n == 0) this else tl().drop(n - 1)
    }

    // exercise 5.3

    def takeWhile(f: A => Boolean): Stream[A] = this match {
      case Cons(h, tl) if f(h()) => Cons(h, () => tl().takeWhile(f))
      case _ => Empty
    }

    // exercise 5.4

    def exists(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
      case Empty => z
      case Cons(hd, tl) => f(hd(), tl().foldRight(z)(f))
    }

    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    // exercise 5.5

    def takeWhileFoldRigth(f: A => Boolean): Stream[A] =
      foldRight(Empty: Stream[A])((a, b) => if (f(a)) Stream.cons(a, b) else b)

    // exercise 5.6

    def headOptionFoldRight(): Option[A] =
      foldRight(None: Option[A])((a, _) => Some(a))

    // exercise 5.7

    def map[B](f: A => B): Stream[B] =
      foldRight(Empty: Stream[B])((a, b) => Stream.cons(f(a), b))

    def filter(f: A => Boolean): Stream[A] =
      foldRight(Empty: Stream[A])((a, b) => if (f(a)) Stream.cons(a, b) else b)

    def append[AA >: A](s: Stream[AA]): Stream[AA] =
      foldRight(s)((a, b) => Stream.cons(a, b))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(Empty: Stream[B])((a, b) => f(a).append(b))

    // exercise 5.13

    def mapUnfold[B](f: A => B): Stream[B] = Stream.unfold(this) {
      case Empty => None
      case Cons(hd, tl) => Some((f(hd()), tl()))
    }

    def takeUnfold(n: Int): Stream[A] = Stream.unfold((this, n)) {
      case (stream, count) =>
        if (count == 0) None
        else stream match {
          case Empty => None
          case Cons(hd, tl) => Some((hd(), (tl(), count - 1)))
        }
    }

    def takeWhileUnfold(f: A => Boolean): Stream[A] = Stream.unfold(this) {
      case Cons(hd, tl) if f(hd()) => Some((hd(), tl()))
      case _ => None
    }





  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {

    def cons[A](h: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = h
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

    // exercise 5.8

    def constant[A](a: A): Stream[A] = {
      lazy val constant: Stream[A] = cons(a, constant)
      constant
    }

    // exercise 5.9

    def from(n: Int): Stream[Int] = {
      cons(n, from(n + 1))
    }

    // exercise 5.10

    def fibs: Stream[Int] = {
      def go(i: Int, j: Int): Stream[Int] = cons(i, go(j, i + j))

      go(0, 1)
    }

    // exercise 5.11

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }

    // exercise 5.12

    def constantUnfold[A](a: A): Stream[A] = unfold(a)(s => Some(s, a))

    def fromUnfold(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

    def fibsUnfold: Stream[Int] = unfold((0, 1)) { case (f0, f1) => Some(f0, (f1, f0 + f1)) }

  }

  def main(args: Array[String]): Unit = {
    lazy val ones: Stream[Int] = Stream.cons(1, ones)
    println(ones.take(5).toList)
    println(ones.map(_ + 1).exists(_ % 2 == 0))
    println(ones.takeWhile(_ == 1))
    println(ones.forAll(_ != 1))
  }

}
