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

  // Exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    val m = mean(xs).getOrElse(0.0)
    mean(xs.map(x => math.pow(x - m, 2)))
  }

  // Exercise 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (None, None) => None
    case (None, Some(_)) => None
    case (Some(_), None) => None
    case (Some(va), Some(vb)) => Some(f(va, vb))
  }

  // Exercise 4.4
  def sequence[A](as: List[Option[A]]): Option[List[A]] = {

    def go[A](list: List[Option[A]], result: List[A]): List[A] = list match {
      case Nil => Nil
      case None :: _ => Nil
      case Some(h) :: t => h :: go(t, result)
    }

    val result = go(as, List())
    if (result.size == as.size) {
      Some(result)
    } else {
      None
    }
  }
}

