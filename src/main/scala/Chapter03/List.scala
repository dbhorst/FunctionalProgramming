package Chapter03

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 3.2
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  // Exercise 3.3
  def setHead[A](as: List[A], head: A): List[A] = as match {
    case Nil => List(head)
    case Cons(_, tail) => Cons(head, tail)
  }

  // Exercise 3.4
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0 || l == List()) l
    else drop(tail(l), n - 1)

  // Exercise 3.5
  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(head, tail) if f(head) => dropWhile(tail, f)
    case _ => l
  }

  def dropWhileCurry[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Cons(head, tail) if f(head) => dropWhile(tail, f)
    case _ => l
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) => f(h, foldRight(t, z)(f))
  }

  def sum2(ns: List[Int]) = {
    foldRight(ns, 0)(_ + _)
  }

  def product2(ns: List[Double]) = {
    foldRight(ns, 1.0)(_ * _)
  }

  // Exercise 3.9
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, y) => y + 1)
  }

  // Exercise 3.10
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // Exercise 3.11
  def sum3(ns: List[Int]) = {
    foldLeft(ns, 0)(_ + _)
  }

  def product3(ns: List[Double]) = {
    foldLeft(ns, 1.0)(_ * _)
  }

  def length2[A](as: List[A]): Int = {
    foldLeft(as, 0)((x, _) => x + 1)
  }

  // Exercise 3.12
  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, List(): List[A])((x, y) => Cons(y, x))
  }

  // Exercise 3.14
  def append2[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)((x, y) => Cons(x, y))
  }

  // Exercise 3.15
  def concat[A](ass: List[List[A]]): List[A] = ass match {
    case Nil => Nil
    case Cons(h, Nil) => h
    case Cons(h, t) => append2(h, concat(t))
  }

  // Exercise 3.16
  def transform(as: List[Int]): List[Int] = as match {
    case Nil => Nil
    case Cons(h, Nil) => Cons(h + 1, Nil)
    case Cons(h, t) => Cons(h + 1, transform(t))
  }

  def transform2(as: List[Int]): List[Int] = {
    foldRight(as, List(): List[Int])((x, y) => Cons(x + 1, y))
  }

  // Exercise 3.17
  def doubleToString(as: List[Double]): List[String] = {
    foldRight(as, List(): List[String])((x, y) => Cons(x.toString, y))
  }

  // Exercise 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = {
    foldRight(as, List(): List[B])((x, y) => Cons(f(x), y))
  }

  // Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight(as, List(): List[A])((x, y) => if (f(x)) y else Cons(x, y))
  }

  // Exercise 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, List(): List[B])((x, y) => append2(f(x), y))
  }

  // Exercise 3.21
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)((x) => if (f(x)) List() else List(x))
  }

  // Exercise 3.22
  def addLists(as1: List[Int], as2: List[Int]): List[Int] = (as1, as2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addLists(t1, t2))
  }

  // Exercise 3.23
  def zipWith[A](as1: List[A], as2: List[A])(f:(A, A) => A): List[A] = (as1, as2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

}