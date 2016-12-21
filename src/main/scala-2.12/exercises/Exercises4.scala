package exercises

/**
  * FP in Scala : Chapter 4
  *
  * @author lepnio
  * @project fp-scala
  */
object Exercises4 {

  // Option

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

  def mean(xs: Seq[Double]): Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs) flatMap (m => mean(xs map (x => math.pow(x - m, 2))))

  // exercise 4.3

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(v1 => b.map(v2 => f(v1, v2)))

  // exercise 4.4

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a.foldRight[Option[List[A]]](Some(Nil))((op, opl) => opl.flatMap(l => op.map(a => a :: l)))

  // exercise 4.5

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a.foldRight[Option[List[B]]](Some(Nil))((a, opl) => opl.flatMap(l => f(a).map(b => b :: l)))

  // Either

  sealed trait Either[+E, +A] {

    // exercise 4.6

    def map[B](f: A => B): Either[E, B] = flatMap(a => Right(f(a)))

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Right(a) => f(a)
      case Left(e) => Left(e)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => b
      case r => r;
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = flatMap(va => b.map(vb => f(va, vb)))

  }

  case class Left[+E](value: E) extends Either[E, Nothing]

  case class Right[+A](value: A) extends Either[Nothing, A]

  // exercise 4.7

  def sequenceEither[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es.foldRight[Either[E, List[A]]](Right(Nil))((ea, es) => ea.map2(es)(_ :: _))

  def traverseEither[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as.foldRight[Either[E, List[B]]](Right(Nil))((a, es) => f(a).map2(es)(_ :: _))

}
