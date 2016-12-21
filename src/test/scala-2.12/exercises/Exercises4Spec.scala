package exercises

import exercises.Exercises4._
import org.scalatest._

/**
  * FP in Scala : Chapter 4
  *
  * @author lepnio
  * @project fp-scala
  */
class Exercises4Spec extends FlatSpec {

  // exercise 4.1

  "map None and a => a" should "return None" in {
    assert(None.map { a => a } == None)
  }

  "map Some(1) and a => a * 2" should "return Some(2)" in {
    assert(Some(1).map { a => a * 2 } == Some(2))
  }

  "flatMap None and a => Some(a)" should "return None" in {
    assert(None.flatMap { a => Some(a) } == None)
  }

  "flatMap Some(1) and a => Some(a * 2)" should "return Some(2)" in {
    assert(Some(1).flatMap { a => Some(a * 2) } == Some(2))
  }

  "getOrElse None and 1" should "return 1" in {
    assert(None.getOrElse(1) == 1)
  }

  "getOrElse Some(2) and 1" should "return 2" in {
    assert(Some(2).getOrElse(1) == 2)
  }

  "orElse None and Some(1)" should "return Some(1)" in {
    assert(None.orElse(Some(1)) == Some(1))
  }

  "orElse Some(2) and Some(1)" should "return Some(2)" in {
    assert(Some(2).orElse(Some(1)) == Some(2))
  }

  "filter None and a => a != Nil" should "return None" in {
    assert(None.filter { a => a != Nil } == None)
  }

  "filter Some(1) and a => a > 2" should "return None" in {
    assert(Some(1).filter { a => a > 2 } == None)
  }

  "filter Some(1) and a => a < 2" should "return None" in {
    assert(Some(1).filter { a => a < 2 } == Some(1))
  }

  // exercise 4.2

  "mean of Nil" should "return None" in {
    assert(mean(Nil) == None)
  }

  "mean of List(1,2,3)" should "return Some(2)" in {
    assert(mean(List(1, 2, 3)) == Some(2))
  }

  "variance of Nil" should "return None" in {
    assert(variance(Nil) == None)
  }

  "variance of List(1,2,3,4,5)" should "return Some(2)" in {
    assert(variance(List(1, 2, 3, 4, 5)) == Some(2))
  }

  // exercise 4.3

  "map2 of None, None and (a,b) => a + b" should "return None" in {
    assert(map2(None: Option[Int], None)((a, b) => a + b) == None)
  }

  "map2 of None, Some(1) and (a,b) => a + b" should "return None" in {
    assert(map2(None: Option[Int], Some(1))((a, b) => a + b) == None)
  }

  "map2 of Some(1), None and (a,b) => a + b" should "return None" in {
    assert(map2(Some(1), None)((a, b) => a + b) == None)
  }

  "map2 of Some(1), Some(2) and (a,b) => a + b" should "return Some(3)" in {
    assert(map2(Some(1), Some(2))((a, b) => a + b) == Some(3))
  }

  // exerciqe 4.4

  "sequence of List(None)" should "return None" in {
    assert(sequence(List(None)) == None)
  }

  "sequence of List(Some(1),Some(2),Some(3))" should "return Some(List(1,2,3))" in {
    assert(sequence(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3)))
  }

  "sequence of List(Some(1),None,Some(2))" should "return None" in {
    assert(sequence(List(Some(1), None, Some(2))) == None)
  }

  // exercise 4.5

  "traverse of List(1,2,3) and a => None" should "return None" in {
    assert(traverse(List(1,2,3))(a => None) == None)
  }

  "traverse of Nil and a => None" should "return Some(Nil)" in {
    assert(traverse(Nil)(a => None) == Some(Nil))
  }

  "traverse of List(1,2,3) and a => Some(a)" should "return Some(List(1,2,3))" in {
    assert(traverse(List(1,2,3))(a => Some(a)) == Some(List(1,2,3)))
  }

  "traverse of Nil and a => Some(a)" should "return Some(Nil)" in {
    assert(traverse(Nil)(a => Some(a)) == Some(Nil))
  }

  "traverse of List(1,2,3) and a => if (a < 2) Some(a) else None" should "return None" in {
    assert(traverse(List(1,2,3))(a => if (a < 2) Some(a) else None) == None)
  }

  // exercise 4.6

  "map Left(1) and a => 3" should "return Left(1)" in {
    assert(Left(1).map { a => 3 } == Left(1))
  }

  "map Right(1) and a => a * 2" should "return Right(2)" in {
    assert(Right(1).map { a => a * 2 } == Right(2))
  }

  "flatMap Left(1) and a => Left(3)" should "return Left(1)" in {
    assert(Left(1).flatMap { a => Left(3) } == Left(1))
  }

  "flatMap Right(1) and a => Right(a * 2)" should "return Right(2)" in {
    assert(Right(1).flatMap { a => Right(a * 2) } == Right(2))
  }

  "orElse Left(1) and Right(2)" should "return Right(2)" in {
    assert(Left(1).orElse(Right(2)) == Right(2))
  }

  "orElse Right(2) and Right(1)" should "return Right(2)" in {
    assert(Right(2).orElse(Right(1)) == Right(2))
  }

  "map2 of Left(1), Left(2) and (a,b) => 3" should "return Left(1)" in {
    assert(Left(1).map2(Left(2))((a, b) => 3) == Left(1))
  }

  "map2 of Left(1), Right(2) and (a,b) => 3" should "return Left(1)" in {
    assert(Left(1).map2(Right(2))((a, b) => 3) == Left(1))
  }

  "map2 of Right(1), Left(2) and (a,b) => 3" should "return None" in {
    assert(Right(1).map2(Left(2))((a, b) => 3) == Left(2))
  }

  "map2 of Right(1), Right(2) and (a,b) => 3" should "return Right(3)" in {
    assert(Right(1).map2(Right(2))((a, b) => 3) == Right(3))
  }

  // exerciqe 4.7

  "sequenceEither of List(Left(1), Left(2))" should "return Left(1)" in {
    assert(sequenceEither(List(Left(1), Left(2))) == Left(1))
  }

  "sequenceEither of List(Right(1),Right(2),Right(3))" should "return Right(List(1,2,3))" in {
    assert(sequenceEither(List(Right(1), Right(2), Right(3))) == Right(List(1, 2, 3)))
  }

  "sequenceEither of List(Right(1),Left(1),Right(2))" should "return Left(1)" in {
    assert(sequenceEither(List(Right(1), Left(1), Right(2))) == Left(1))
  }

  "traverseEither of List(1,2,3) and a => Left(a)" should "return Left(1)" in {
    assert(traverseEither(List(1,2,3))(a => Left(a)) == Left(1))
  }

  "traverseEither of Nil and a => Left(a)" should "return Right(Nil)" in {
    assert(traverseEither(Nil)(a => Left(a)) == Right(Nil))
  }

  "traverseEither of List(1,2,3) and a => Right(a)" should "return Right(List(1,2,3))" in {
    assert(traverseEither(List(1,2,3))(a => Right(a)) == Right(List(1,2,3)))
  }

  "traverseEither of Nil and a => Right(a)" should "return Right(Nil)" in {
    assert(traverseEither(Nil)(a => Right(a)) == Right(Nil))
  }

  "traverseEither of List(1,2,3) and a => if (a < 2) Right(a) else Left(a)" should "return Left(2)" in {
    assert(traverseEither(List(1,2,3))(a => if (a < 2) Right(a) else Left(a)) == Left(2))
  }

}