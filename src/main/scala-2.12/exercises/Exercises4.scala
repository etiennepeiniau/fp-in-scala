package exercises

/**
  * FP in Scala : Chapter 4
  *
  * @author lepnio
  * @project fp-scala
  */
object Exercises4 {

  // exercise 4.1

  sealed trait Option[+A] {

    def map[B](f: A => B): Option[B] = this flatMap { a => Some(f(a)) }

    def flatMap[B](f: A => Option[B]): Option[B] = this match {
      case None => None
      case Some(a) => f(a)
    }

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(a) => a
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse ob

    def filter(f: A => Boolean): Option[A] = this.flatMap { a => if (f(a)) Some(a) else None }

  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  // exercice 4.2

  def mean(xs: Seq[Double]): Option[Double] = xs.foldLeft(None: Option[Double])((op, value) => op.map(a => a + value).orElse(Some(value))).map(sum => sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs) flatMap (m => mean(xs map (x => math.pow(x - m, 2))))

}
