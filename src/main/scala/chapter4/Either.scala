package chapter4

/**
  * The generic E mean Error
  *
  * Further information, in the scala api doc
  *
  * @author loustler
  * @since 08/12/2017 18:22
  */
sealed trait Either[+E, +A] extends Commons {
  // exercise 4.6
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(e) => Right(f(e))
    case Left(e)  => Left(e)
  }

  // exercise 4.6
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e)  => Left(e)
    case Right(e) => f(e)
  }

  // exercise 4.6
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(e)  => b
    case Right(e) => this
  }

  // exercise 4.6
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      e1 <- this
      e2 <- b
    } yield f(e1, e2)
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

protected[this] sealed trait Commons {
  def Try[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }
  }
}

object Either extends Commons {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] = {
    if (xs.isEmpty)
      Left("mean of empty list")
    else
      Right(xs.sum / xs.length)
  }

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = {
    Try(x / y)
  }
}
