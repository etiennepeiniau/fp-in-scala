package exercises

import exercises.Exercises3.List._
import exercises.Exercises3.Tree._
import exercises.Exercises3._
import org.scalatest._

/**
  * FP in Scala : Chapter 3
  *
  * @author lepnio
  * @project fp-scala
  */
class Exercises3Spec extends FlatSpec {

  // exercise 3.1

  "pattern matching on List(1, 2, 3, 4, 5)" should "return 3" in {
    val x = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
  }

  // exercise 3.2

  "tail on Nil" should "throw an exception" in {
    assertThrows[RuntimeException] {
      tail(Nil)
    }
  }

  "tail on List(1, 2, 3, 4, 5)" should "return List(2, 3, 4, 5)" in {
    assert(tail(List(1, 2, 3, 4, 5)) == List(2, 3, 4, 5))
  }

  // exercise 3.3

  "setHead 1 on Nil" should "throw an exception" in {
    assertThrows[RuntimeException] {
      setHead(Nil, 1)
    }
  }

  "setHead 2 on List(1, 2, 3, 4, 5)" should "return List(2, 2, 3, 4, 5)" in {
    assert(setHead(List(1, 2, 3, 4, 5), 2) == List(2, 2, 3, 4, 5))
  }

  // exercise 3.4

  "drop 2 on Nil" should "return Nil" in {
    assert(drop(Nil, 2) == Nil)
  }

  "drop 2 on List(1, 2, 3, 4, 5)" should "return List(3, 4, 5)" in {
    assert(drop(List(1, 2, 3, 4, 5), 2) == List(3, 4, 5))
  }

  // exercise 3.5

  "dropWhile < 4 on Nil" should "return Nil" in {
    assert(dropWhile(Nil, (a: Int) => a < 4) == Nil)
  }

  "dropWhile < 4 List(1, 2, 3, 4, 5)" should "return List(4, 5)" in {
    assert(dropWhile(List(1, 2, 3, 4, 5), (a: Int) => a < 4) == List(4, 5))
  }

  // exercise 3.6

  "init on Nil" should "throw an exception" in {
    assertThrows[RuntimeException] {
      init(Nil)
    }
  }

  "init on List(1)" should "return Nil" in {
    assert(init(List(1)) == Nil)
  }

  "init on List(1, 2, 3, 4, 5)" should "return List(1, 2, 3, 4)" in {
    assert(init(List(1, 2, 3, 4, 5)) == List(1, 2, 3, 4))
  }

  "init tail rec on Nil" should "throw an exception" in {
    assertThrows[RuntimeException] {
      initTailRec(Nil)
    }
  }

  "init tail rec on List(1)" should "return Nil" in {
    assert(initTailRec(List(1)) == Nil)
  }

  "init tail rec on List(1, 2, 3, 4, 5)" should "return List(1, 2, 3, 4)" in {
    assert(initTailRec(List(1, 2, 3, 4, 5)) == List(1, 2, 3, 4))
  }

  // exercise 3.8

  "foldRight" should "copy List(1,2,3)" in {
    assert(foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) == List(1, 2, 3))
  }

  // exercise 3.9

  "length of Nil" should "return 0" in {
    assert(length(Nil) == 0)
  }

  "length of List(1,2,3)" should "return 3" in {
    assert(length(List(1, 2, 3)) == 3)
  }

  // exercise 3.11

  "sumFoldLeft of Nil" should "return 0" in {
    assert(sumFoldLeft(Nil) == 0)
  }

  "sumFoldLeft of List(1,2,3,4)" should "return 10" in {
    assert(sumFoldLeft(List(1, 2, 3, 4)) == 10)
  }

  "productFoldLeft of Nil" should "return 1" in {
    assert(productFoldLeft(Nil) == 1)
  }

  "productFoldLeft of List(1,2,3,4)" should "return 24" in {
    assert(productFoldLeft(List(1, 2, 3, 4)) == 24)
  }

  "lengthFoldLeft of Nil" should "return 0" in {
    assert(lengthFoldLeft(Nil) == 0)
  }

  "lengthFoldLeft of List(1,2,3,4)" should "return 4" in {
    assert(lengthFoldLeft(List(1, 2, 3, 4)) == 4)
  }

  // excercise 3.12

  "reverse of Nil" should "return Nil" in {
    assert(reverse(Nil) == Nil)
  }

  "reverse of List(1,2,3)" should "return List(3,2,1)" in {
    assert(reverse(List(1, 2, 3)) == List(3, 2, 1))
  }

  // exercise 3.13

  "foldRightRec" should "copy List(1,2,3)" in {
    assert(foldRightRec(List(1, 2, 3), Nil: List[Int])(Cons(_, _)) == List(1, 2, 3))
  }

  // exercise 3.14

  "append of Nil and Nil" should "return Nil" in {
    assert(append(Nil, Nil) == Nil)
  }

  "append of Nil and List(1,2,3)" should "return List(1,2,3)" in {
    assert(append(Nil, List(1, 2, 3)) == List(1, 2, 3))
  }

  "append of List(1,2,3) and Nil" should "return List(1,2,3)" in {
    assert(append(List(1, 2, 3), Nil) == List(1, 2, 3))
  }

  "append of List(1,2,3) and List(4,5,6)" should "return List(1,2,3,4,5,6" in {
    assert(append(List(1, 2, 3), List(4, 5, 6)) == List(1, 2, 3, 4, 5, 6))
  }

  // exercise 3.15

  "flatten of List(Nil, Nil" should "return Nil" in {
    assert(flatten(List(Nil, Nil)) == Nil)
  }

  "flatten of List(List(1,2,3), Nil" should "return List(1,2,3)" in {
    assert(flatten(List(List(1, 2, 3), Nil)) == List(1, 2, 3))
  }

  "flatten of List(Nil, List(4,5,6))" should "return List(4,5,6)" in {
    assert(flatten(List(Nil, List(4, 5, 6))) == List(4, 5, 6))
  }

  "flatten of List(List(1,2,3), List(4,5,6))" should "return List(1,2,3,4,5,6)" in {
    assert(flatten(List(List(1, 2, 3), List(4, 5, 6))) == List(1, 2, 3, 4, 5, 6))
  }

  // exercise 3.16

  "addOne of Nil" should "return Nil" in {
    assert(addOne(Nil) == Nil)
  }

  "addOne of List(2,4,6)" should "return List(3,5,7)" in {
    assert(addOne(List(2, 4, 6)) == List(3, 5, 7))
  }

  // exercise 3.17

  "doubleToString of Nil" should "return Nil" in {
    assert(doubleToString(Nil) == Nil)
  }

  "doubleToString of List(2d,4d,6d)" should "return List('2.0','4.0','6.0')" in {
    assert(doubleToString(List(2d, 4d, 6d)) == List("2.0", "4.0", "6.0"))
  }

  // exercise 3.18

  "map of Nil and a => a + 1" should "return Nil" in {
    assert(map(Nil: List[Int])(a => a + 1) == Nil)
  }

  "map of List(2,4,6) and a => a + 1" should "return List(3,5,7)" in {
    assert(map(List(2, 4, 6))(a => a + 1) == List(3, 5, 7))
  }

  // exercise 3.19

  "filter of Nil and a => a % 2 != 0" should "return Nil" in {
    assert(filter(Nil: List[Int])(a => a % 2 != 0) == Nil)
  }

  "filter of List(2,3,4) and a => a % 2 != 0" should "return List(3)" in {
    assert(filter(List(2, 3, 4))(a => a % 2 != 0) == List(3))
  }

  // exercise 3.20

  "flatMap of Nil and i => List(i,i)" should "return Nil" in {
    assert(flatMap(Nil: List[Int])(i => List(i, i)) == Nil)
  }

  "flatMap of List(1,2,3) and i => List(i,i)" should "return List(1,1,2,2,3,3)" in {
    assert(flatMap(List(1, 2, 3))(i => List(i, i)) == List(1, 1, 2, 2, 3, 3))
  }

  // exercise 3.21

  "filterAsFlatMap of Nil and a => a % 2 != 0" should "return Nil" in {
    assert(filterAsFlatMap(Nil: List[Int])(a => a % 2 != 0) == Nil)
  }

  "filterAsFlatMap of List(2,3,4) and a => a % 2 != 0" should "return List(3)" in {
    assert(filterAsFlatMap(List(2, 3, 4))(a => a % 2 != 0) == List(3))
  }

  // exercise 3.22

  "zipIntList of Nil and Nil" should "return Nil" in {
    assert(zipIntList(Nil, Nil) == Nil)
  }

  "zipIntList of List(1,2,3) and Nil" should "return List(1,2,3)" in {
    assert(zipIntList(List(1, 2, 3), Nil) == List(1, 2, 3))
  }

  "zipIntList of Nil nad List(4,5,6)" should "return Nil" in {
    assert(zipIntList(Nil, List(4, 5, 6)) == List(4, 5, 6))
  }

  "zipIntList of List(1,2,3) and List(4,5,6)" should "return List(5,7,9)" in {
    assert(zipIntList(List(1, 2, 3), List(4, 5, 6)) == List(5, 7, 9))
  }

  "zipIntList of Lost(1,2,3,4) and List(4,5,6)" should "return List(5,7,9)" in {
    assert(zipIntList(List(1, 2, 3, 4), List(4, 5, 6)) == List(5, 7, 9, 4))
  }

  "zipIntList of List(1,2,3) and List(4,5,6,7)" should "return List(5,7,9,7)" in {
    assert(zipIntList(List(1, 2, 3), List(4, 5, 6, 7)) == List(5, 7, 9, 7))
  }

  // exercise 3.23

  "zipList of Nil and Nil with (_ + _)" should "return Nil" in {
    assert(zipList(Nil: List[Int], Nil: List[Int])(_ + _) == Nil)
  }

  "zipList of List(1,2,3) and Nil with (_ + _)" should "return List(1,2,3)" in {
    assert(zipList(List(1, 2, 3), Nil)(_ + _) == List(1, 2, 3))
  }

  "zipList of Nil nad List(4,5,6) with (_ + _)" should "return Nil" in {
    assert(zipList(Nil, List(4, 5, 6))(_ + _) == List(4, 5, 6))
  }

  "zipList of List(1,2,3) and List(4,5,6) with (_ + _)" should "return List(5,7,9)" in {
    assert(zipList(List(1, 2, 3), List(4, 5, 6))(_ + _) == List(5, 7, 9))
  }

  "zipList of List(1,2,3,4) and List(4,5,6) with (_ + _)" should "return List(5,7,9)" in {
    assert(zipList(List(1, 2, 3, 4), List(4, 5, 6))(_ + _) == List(5, 7, 9, 4))
  }

  "zipList of List(1,2,3) and List(4,5,6,7) with (_ + _)" should "return List(5,7,9,7)" in {
    assert(zipList(List(1, 2, 3), List(4, 5, 6, 7))(_ + _) == List(5, 7, 9, 7))
  }

  // exercise 3.24

  "hasSubsequence of List(1,2,3,4) and Nil" should "return true" in {
    assert(hasSubsequence(List(1, 2, 3, 4), Nil))
  }

  "hasSubsequence of List(1,2,3,4) and List(1,2)" should "return true" in {
    assert(hasSubsequence(List(1, 2, 3, 4), List(1, 2)))
  }

  "hasSubsequence of List(1,2,3,4) and List(2,3)" should "return true" in {
    assert(hasSubsequence(List(1, 2, 3, 4), List(2, 3)))
  }

  "hasSubsequence of List(1,2,3,4) and List(4)" should "return true" in {
    assert(hasSubsequence(List(1, 2, 3, 4), List(4)))
  }

  "hasSubsequence of List(1,2,3,4) and List(2, 1)" should "return false" in {
    assert(!hasSubsequence(List(1, 2, 3, 4), List(2, 1)))
  }

  "hasSubsequence of List(1,2,3,4) and List(2, 4)" should "return false" in {
    assert(!hasSubsequence(List(1, 2, 3, 4), List(2, 4)))
  }

  // exercise 3.25

  "size of Leaf(1)" should "return 1" in {
    assert(size(Leaf(1)) == 1)
  }

  "size of Branch(Leaf(1), Branch(Leaf(1),Leaf(1)))" should "return 5" in {
    assert(size(Branch(Leaf(1), Branch(Leaf(1), Leaf(1)))) == 5)
  }

  // exercise 3.26

  "maximum of Leaf(3)" should "return 3" in {
    assert(maximum(Leaf(3)) == 3)
  }

  "maximum of Branch(Leaf(3), Branch(Leaf(1),Leaf(6)))" should "return 6" in {
    assert(maximum(Branch(Leaf(3), Branch(Leaf(1), Leaf(6)))) == 6)
  }

  // exercise 3.27

  "depth of Leaf(3)" should "return 1" in {
    assert(depth(Leaf(3)) == 1)
  }

  "depth of Branch(Leaf(3), Branch(Leaf(1),Leaf(6)))" should "return 3" in {
    assert(depth(Branch(Leaf(3), Branch(Leaf(1), Leaf(6)))) == 3)
  }

}
