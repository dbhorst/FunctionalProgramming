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
  def max(tree: Tree[Int]): Int = tree match {
    case Leaf(x) => x
    case Branch(l, r) => max(l) max max(r)
  }

  // Execise 3.27
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(x) => 1
    case Branch(l, r) => (depth(l) max depth(r)) + 1
  }

  // Exercise 3.28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

}