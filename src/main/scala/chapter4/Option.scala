package chapter4

/**
  * @author loustler
  * @since 07/23/2017 16:35
  */
object Option {
  def failingFn(i: Int): Int = {
    // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    val y: Int = throw new Exception("fail!")
    try {
      val x = 42 + 5
      x + y
    }
    // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern
    // that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
    catch { case e: Exception => 43 }
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      // A thrown Exception can be given any type; here we're annotating it with the type `Int`
      x + ((throw new Exception("fail!")): Int)
    } catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def apply[A](a: A): Option[A] = {
    if (a == null)
      None
    else
      Some(a)
  }

  // exercise 4-2
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (d => mean(xs.map(x => math.pow(x - d, 2))))
  }
}

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
