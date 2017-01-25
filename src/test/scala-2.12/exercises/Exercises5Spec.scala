package exercises

import exercises.Exercises5.Stream._
import exercises.Exercises5._
import org.scalatest._

/**
  * FP in Scala : Chapter 5
  *
  * @author lepnio
  * @project fp-scala
  */
class Exercises5Spec extends FlatSpec {

  // exercise 5.1

  "toList of Empty" should "return Nil" in {
    assert(Empty.toList == Nil)
  }

  "toList of Stream(1,2,3,4)" should "return List(1,2,3,4)" in {
    assert(Stream(1, 2, 3, 4).toList == List(1, 2, 3, 4))
  }

  "toListRec of Empty" should "return Nil" in {
    assert(Empty.toListRec == Nil)
  }

  "toListRec of Stream(1,2,3,4)" should "return List(1,2,3,4)" in {
    assert(Stream(1, 2, 3, 4).toListRec == List(1, 2, 3, 4))
  }

  // exercise 5.2

  "take(3) of Empty" should "return Empty" in {
    assert(Empty.take(3) == Empty)
  }

  "take(0) of Stream(1,2,3,4)" should "return Empty" in {
    assert(Stream(1, 2, 3, 4).take(0) == Empty)
  }

  "take(3) of Stream(1,2,3,4)" should "return Stream(1,2,3)" in {
    assert(Stream(1, 2, 3, 4).take(3).toList == List(1, 2, 3))
  }

  "take(5) of Stream(1,2,3,4)" should "return Stream(1,2,3,4)" in {
    assert(Stream(1, 2, 3, 4).take(5).toList == List(1, 2, 3, 4))
  }

  "drop(3) of Empty" should "return Nil" in {
    assert(Empty.drop(3) == Empty)
  }

  "drop(0) of Stream(1,2,3,4)" should "return Empty" in {
    assert(Stream(1, 2, 3, 4).drop(0).toList == List(1, 2, 3, 4))
  }

  "drop(3) of Stream(1,2,3,4)" should "return Stream(4)" in {
    assert(Stream(1, 2, 3, 4).drop(3).toList == List(4))
  }

  "drop(5) of Stream(1,2,3,4)" should "return Empty" in {
    assert(Stream(1, 2, 3, 4).drop(5) == Empty)
  }

  // exercise 5.3


  "takeWhile(a => a > 2) of empty[Int]" should "return Empty" in {
    assert(empty[Int].takeWhile(a => a > 2) == Empty)
  }

  "takeWhile(a => a < 3) of Stream(1,2,3,4)" should "return Stream(1,2,3)" in {
    assert(Stream(1, 2, 3, 4).takeWhile(a => a < 3).toList == List(1, 2))
  }

  "takeWhile(a => a < 10) of Stream(1,2,3,4)" should "return Stream(1,2,3,4)" in {
    assert(Stream(1, 2, 3, 4).takeWhile(a => a < 10).toList == List(1, 2, 3, 4))
  }

  // exercise 5.4

  "foldRight of Stream(1,2,3,4), '5' and (a, b) => a + '-' + b" should "return '1-2-3-4'" in {
    assert(Stream(1, 2, 3, 4).foldRight(() => "5")((a, b) => () => a + "-" + b())() == "1-2-3-4-5")
  }

  "forAll of Stream(1,2,3,4) and a => a < 10" should "return true" in {
    assert(Stream(1, 2, 3, 4).forAll(_ < 10))
  }

  "forAll of Stream(1,2,3,4) and a => a < 3" should "return false" in {
    assert(!Stream(1, 2, 3, 4).forAll(_ < 3))
  }

  // exercise 5.5

  "takeWhileFoldRigth(a => a > 2) of empty[Int]" should "return Empty" in {
    assert(empty[Int].takeWhileFoldRigth(a => a > 2) == Empty)
  }

  "takeWhileFoldRigth(a => a < 3) of Stream(1,2,3,4)" should "return Stream(1,2,3)" in {
    assert(Stream(1, 2, 3, 4).takeWhileFoldRigth(a => a < 3).toList == List(1, 2))
  }

  "takeWhileFoldRigth(a => a < 10) of Stream(1,2,3,4)" should "return Stream(1,2,3,4)" in {
    assert(Stream(1, 2, 3, 4).takeWhileFoldRigth(a => a < 10).toList == List(1, 2, 3, 4))
  }

  // exercise 5.6

  "headOptionFoldRight of Empty" should "return None" in {
    assert(Empty.headOptionFoldRight().isEmpty)
  }

  "headOptionFoldRight of Stream(1,2,3,4)" should "return Some(1)" in {
    assert(Stream(1, 2, 3, 4).headOptionFoldRight().contains(1))
  }

  // exercise 5.7

  "map of Empty and () => 1" should "return Empty" in {
    assert(Empty.map((a) => 1) == Empty)
  }

  "map of Stream(1,2,3,4) and () => 1" should "return Stream(1,1,1,1)" in {
    assert(Stream(1, 2, 3, 4).map((a) => 1).toList == List(1, 1, 1, 1))
  }

  "filter of Empty and a => a % 2 == 0" should "return Empty" in {
    assert(empty[Int].filter(a => a % 2 == 0) == Empty)
  }

  "filter of Stream(1,2,3,4) and a => a % 2 == 0" should "return Stream(2,4)" in {
    assert(Stream(1, 2, 3, 4).filter(a => a % 2 == 0).toList == List(2, 4))
  }

  "append of Empty and Empty" should "return Empty" in {
    assert(Empty.append(Empty) == Empty)
  }

  "append of Empty and Stream(1,2,3)" should "return Stream(1,2,3)" in {
    assert(Empty.append(Stream(1, 2, 3)).toList == List(1, 2, 3))
  }

  "append of Stream(1,2,3) and Empty" should "return Stream(1,2,3)" in {
    assert(Stream(1, 2, 3).append(Empty).toList == List(1, 2, 3))
  }

  "append of Stream(1,2,3) and Stream(4,5,6)" should "return Stream(1,2,3,4,5,6" in {
    assert(Stream(1, 2, 3).append(Stream(4, 5, 6)).toList == List(1, 2, 3, 4, 5, 6))
  }

  "flatMap of Empty and i => Stream(i,i)" should "return Empty" in {
    assert(empty[Stream[Int]].flatMap(i => Stream(i, i)) == Empty)
  }

  "flatMap of Stream(1,2,3) and i => Stream(i,i)" should "return Stream(1,1,2,2,3,3)" in {
    assert(Stream(1, 2, 3).flatMap(i => Stream(i, i)).toList == List(1, 1, 2, 2, 3, 3))
  }

  // exercise 5.8

  "constant of 5" should "return Stream(5,5,5,.....)" in {
    assert(constant(5).take(6).toList == List(5, 5, 5, 5, 5, 5))
  }

  // exercise 5.9

  "from of 5" should "return Stream(5,6,7,8,...)" in {
    assert(from(5).take(6).toList == List(5, 6, 7, 8, 9, 10))
  }

  // exercise 5.10

  "fibs" should "return Strean(0,1,1,2,3,5,8,...)" in {
    assert(fibs.take(7).toList == List(0, 1, 1, 2, 3, 5, 8))
  }

  // exercise 5.12

  "constantUnfold of 5" should "return Stream(5,5,5,.....)" in {
    assert(constantUnfold(5).take(6).toList == List(5, 5, 5, 5, 5, 5))
  }

  "fromUnfold of 5" should "return Stream(5,6,7,8,...)" in {
    assert(fromUnfold(5).take(6).toList == List(5, 6, 7, 8, 9, 10))
  }

  "fibsUnfold" should "return Strean(0,1,1,2,3,5,8,...)" in {
    assert(fibsUnfold.take(7).toList == List(0, 1, 1, 2, 3, 5, 8))
  }

  // exercise 5.13

  "mapUnfold of Empty and () => 1" should "return Empty" in {
    assert(Empty.mapUnfold((a) => 1) == Empty)
  }

  "mapUnfold of Stream(1,2,3,4) and () => 1" should "return Stream(1,1,1,1)" in {
    assert(Stream(1, 2, 3, 4).mapUnfold((a) => 1).toList == List(1, 1, 1, 1))
  }

  "takeUnfold(3) of Empty" should "return Empty" in {
    assert(Empty.takeUnfold(3) == Empty)
  }

  "takeUnfold(0) of Stream(1,2,3,4)" should "return Empty" in {
    assert(Stream(1, 2, 3, 4).takeUnfold(0) == Empty)
  }

  "takeUnfold(3) of Stream(1,2,3,4)" should "return Stream(1,2,3)" in {
    assert(Stream(1, 2, 3, 4).takeUnfold(3).toList == List(1, 2, 3))
  }

  "takeUnfold(5) of Stream(1,2,3,4)" should "return Stream(1,2,3,4)" in {
    assert(Stream(1, 2, 3, 4).takeUnfold(5).toList == List(1, 2, 3, 4))
  }

  "takeWhileUnfold(a => a > 2) of empty[Int]" should "return Empty" in {
    assert(empty[Int].takeWhileUnfold(a => a > 2) == Empty)
  }

  "takeWhileUnfold(a => a < 3) of Stream(1,2,3,4)" should "return Stream(1,2,3)" in {
    assert(Stream(1, 2, 3, 4).takeWhileUnfold(a => a < 3).toList == List(1, 2))
  }

  "takeWhileUnfold(a => a < 10) of Stream(1,2,3,4)" should "return Stream(1,2,3,4)" in {
    assert(Stream(1, 2, 3, 4).takeWhileUnfold(a => a < 10).toList == List(1, 2, 3, 4))
  }


}