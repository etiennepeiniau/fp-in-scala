package exercises

import scala.annotation.tailrec

/**
  * FP in Scala : Chapter 2
  *
  * @author lepnio
  * @project fp-scala 
  */
object Exercises2 {

  // exercise 2.1

  def fibo(n: Int): Int = {
    @tailrec
    def go(n: Int, prev: Int, cur: Int): Int =
      if (n <= 0) prev
      else go(n - 1, cur, prev + cur)
    go(n, 0, 1)
  }

  // exercise 2.2

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int, acc: Boolean): Boolean = {
      if (as.length < 2) true
      else if (n + 1 >= as.length) acc
      else loop(n + 1, acc && ordered(as(n), as(n + 1)))
    }
    loop(0, true)
  }

  // exercises 2.3

  def curry[A, B, C](f: (A, B) => C): A => B => C = a => b => f(a, b)

  // exercises 2.4

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  // exercises 2.5

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

}
