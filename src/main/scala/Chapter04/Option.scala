package Chapter04

sealed trait Option[+A] {
  // Execise 4.1
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(value) => Some(f(value))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f).getOrElse(None)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(value) => value
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(_) => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case None => None
    case Some(value) => if (f(value)) this else None
  }
}

case class Some[+A](get: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  }

  // Execise 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    var m = mean(xs).getOrElse(0.0)
    mean(xs.map(x => math.pow(x - m, 2)))
  }
}

