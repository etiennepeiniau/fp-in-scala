package exercises

import exercises.Exercises2._
import org.scalatest._

/**
  * FP in Scala : Chapter 2
  *
  * @author lepnio
  * @project fp-scala 
  */
class Exercises2Spec extends FlatSpec {

  // exercise 2.1

  "Fibo" should "return 0 for n equals 0" in {
    assert(fibo(0) == 0)
  }

  "Fibo" should "return 1 for n equals 1" in {
    assert(fibo(1) == 1)
  }

  "Fibo" should "return 1 for n equals 2" in {
    assert(fibo(2) == 1)
  }

  "Fibo" should "return 2 for n equals 3" in {
    assert(fibo(3) == 2)
  }

  "Fibo" should "return 8 for n equals 6" in {
    assert(fibo(6) == 8)
  }

  // exercise 2.2

  "isSorted" should "return true for an empty integer array" in {
    assert(isSorted(Array.emptyIntArray, (x: Int, y: Int) => x > y))
  }

  "isSorted" should "return true for a one element integer array" in {
    assert(isSorted(Array(1), (x: Int, y: Int) => x > y))
  }

  "isSorted" should "return true for a sorted integer array" in {
    assert(isSorted(Array(1, 3, 5, 6, 8), (x: Int, y: Int) => x < y))
  }

  "isSorted" should "return false for a not sorted integer array" in {
    assert(!isSorted(Array(1, 3, 5, 8, 6), (x: Int, y: Int) => x < y))
  }

}
