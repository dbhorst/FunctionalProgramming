package Chapter03

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // Execise 3.25
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  // Execise 3.26
  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  // Execise 3.27
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  // Exercise 3.28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // Exercise 3.29
  def fold[A, B](tree: Tree[A])(bf: (B, B) => B)(lf: A => B): B = tree match {
    case Leaf(x) => lf(x)
    case Branch(l, r) => bf(fold(l)(bf)(lf), fold(r)(bf)(lf))
  }

  def size2[A](tree: Tree[A]): Int = {
    fold(tree)((l: Int, r: Int) => 1 + l + r)(_ => 1)
  }

  def maximum2(tree: Tree[Int]): Int = {
    fold(tree)((l: Int, r: Int) => l max r)(x => x)
  }

  def depth2[A](tree: Tree[A]): Int = {
    fold(tree)((l: Int, r: Int) => (l max r) + 1)(_ => 1)
  }

  //  def map2[A, B](tree: Tree[A])(f: A => B): Tree[B] = {
  //    fold(tree)((l, r) => Branch(l, r))(x => Leaf(f(x)))
  //  }
}