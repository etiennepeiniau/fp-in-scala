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
    assert(mean(List(1,2,3)) == Some(2))
  }



}