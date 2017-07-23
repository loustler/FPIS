package chapter4

/**
  * @author loustler
  * @since 07/23/2017 16:35
  */
object Option {}

sealed trait Option[+A] {
  // exercise 4.1
  def map[B](f: A => B): Option[B] = this match {
    case None    => None
    case Some(a) => Some(f(a))
  }

  // exercise 4.1
  def getOrElse[B >: A](dv: => B): B = this match {
    case None    => dv
    case Some(a) => a
  }

  // exercise 4.1
  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f).getOrElse(None)
  }

  // exercise 4.1
  def orElse[B >: A](ob: Option[B]): Option[B] = {
    map(Some(_)) getOrElse ob // Some(Some(_)) getOrElse Option[B], Some(_) or Option[B]
  }

  // exercise 4.1
  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => this
    case _               => None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
