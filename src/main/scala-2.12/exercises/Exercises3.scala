package exercises

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * FP in Scala : Chapter 3
  *
  * @author lepnio
  * @project fp-scala
  */
object Exercises3 {

  // List

  sealed trait List[+A]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  case object Nil extends List[Nothing]

  object List {

    def apply[A](as: A*): List[A] = {
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))
    }

    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(hd, tl) => hd + sum(tl)
    }

    // exercise 3.1

    // see the spec class

    // exercise 3.2

    def tail[A](list: List[A]): List[A] = list match {
      case Nil => sys.error("tail of empty list")
      case Cons(_, tl) => tl
    }

    // exercise 3.3

    def setHead[A](l: List[A], hd: A): List[A] = l match {
      case Nil => sys.error("setHead on empty list")
      case Cons(_, tl) => Cons(hd, tl)
    }

    // exercise 3.4

    @tailrec
    def drop[A](l: List[A], n: Int): List[A] = l match {
      case Nil => Nil
      case Cons(hd, tl) => if (n == 0) Cons(hd, tl) else drop(tl, n - 1)
    }

    // exercise 3.5

    @tailrec
    def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(hd, tl) => if (f(hd)) dropWhile(tl, f) else Cons(hd, tl)
    }

    // exercise 3.6

    def init[A](l: List[A]): List[A] = l match {
      case Nil => sys.error("init on empty list")
      case Cons(_, Nil) => Nil
      case Cons(hd, tl) => Cons(hd, init(tl))
    }

    def initTailRec[A](l: List[A]): List[A] = {
      val buf = ListBuffer[A]()

      // local immutable
      @tailrec
      def go(l: List[A]): List[A] = l match {
        case Nil => sys.error("inti on empty list")
        case Cons(_, Nil) => List(buf.toList: _*)
        case Cons(hd, tl) => buf += hd; go(tl)
      }

      go(l)
    }

    // excercice 3.8

    def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

    // exercise 3.9

    def length[A](as: List[A]): Int = foldRight(as, 0)((a, b) => b + 1)

    // exercise 3.10

    @tailrec
    def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

    // exercise 3.11

    def sumFoldLeft(ints: List[Int]): Int = foldLeft(ints, 0)(_ + _)

    def productFoldLeft(doubles: List[Double]): Double = foldLeft(doubles, 1d)(_ * _)

    def lengthFoldLeft[A](as: List[A]) = foldLeft(as, 0)((b, a) => b + 1)

    // exercise 3.12

    def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil: List[A])((b, a) => Cons(a, b))

    // exercise 3.13

    def foldRightRec[A, B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((b, a) => f(a, b))

    // exercise 3.14

    def append[A](as1: List[A], as2: List[A]): List[A] = foldLeft(reverse(as1), as2)((b, a) => Cons(a, b))

    // exercise 3.15 -

    def flatten[A](as: List[List[A]]): List[A] = foldRightRec(as, Nil: List[A])((b, a) => append(b, a))

    // exercise 3.16

    def addOne(as: List[Int]): List[Int] = foldRightRec(as, Nil: List[Int])((a, b) => Cons(a + 1, b))

    // exercise 3.17

    def doubleToString(as: List[Double]): List[String] = foldRightRec(as, Nil: List[String])((a, b) => Cons(a.toString, b))

    // exercise 3.18

    def map[A, B](as: List[A])(f: A => B): List[B] = foldRightRec(as, Nil: List[B])((a, b) => Cons(f(a), b))

    // exercise 3.19

    def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRightRec(as, Nil: List[A])((a, b) => if (f(a)) Cons(a, b) else b)

    // exercise 3.20

    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = foldRightRec(reverse(as), Nil: List[B])((a, b) => append(b, f(a)))

    // exercise 3.21

    def filterAsFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(a => if (f(a)) List(a) else Nil)

    // exercise 3.22

    def zipIntList(as1: List[Int], as2: List[Int]): List[Int] = as1 match {
      case Nil => as2
      case Cons(hd1, tl1) => as2 match {
        case Nil => Cons(hd1, zipIntList(tl1, Nil))
        case Cons(hd2, tl2) => Cons(hd1 + hd2, zipIntList(tl1, tl2))
      }
    }

    // exercise 3.23

    def zipList[A](as1: List[A], as2: List[A])(f: (A, A) => A): List[A] = as1 match {
      case Nil => as2
      case Cons(hd1, tl1) => as2 match {
        case Nil => Cons(hd1, zipList(tl1, Nil)(f))
        case Cons(hd2, tl2) => Cons(f(hd1, hd2), zipList(tl1, tl2)(f))
      }
    }

    // exercice 3.24

    def startSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(hd1, tl1), Cons(hd2, tl2)) => if (hd1 == hd2) startSubsequence(tl1, tl2) else false
    }

    def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(hd1, tl1), Cons(hd2, tl2)) => if (hd1 == hd2) startSubsequence(tl1, tl2) else hasSubsequence(tl1, sub)
    }


  }

  // Tree

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {

    // exercise 3.25

    def size[A](t: Tree[A]): Int = t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

    // exercise 3.26

    def maximum(t: Tree[Int]): Int = t match {
      case Leaf(v) => v
      case Branch(l, r) => {
        val ml = maximum(l)
        val mr = maximum(r)
        if (ml > mr) ml else mr
      }
    }

    // exercise 3.27

    def depth(t: Tree[Int]): Int = t match {
      case Leaf(_) => 1
      case Branch(l, r) => {
        val ldepth = 1 + depth(l)
        val rdepth = 1 + depth(r)
        if (ldepth > rdepth) ldepth else rdepth
      }
    }

    // exercise 3.28

    def mapTree[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l, r) => Branch(mapTree(l)(f), mapTree(r)(f))
    }

    // exercise 3.29

    def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
      case Leaf(v) => f(v)
      case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
    }


    def foldSize[A](t: Tree[A]): Int = fold(t)(a => 1)((b1, b2) => 1 + b1 + b2)

    def foldMaximum(t: Tree[Int]): Int = fold(t)(a => a)((b1, b2) => b1 max b2)

    def foldDepth(t: Tree[Int]): Int = fold(t)(a => 1)((b1, b2) => (1 + b1) max (1 + b2))

    def foldMapTree[A, B](t: Tree[A])(f: A => B): Tree[B] = fold[A, Tree[B]](t)(a => Leaf(f(a)))((b1, b2) => Branch(b1, b2))

  }

}
