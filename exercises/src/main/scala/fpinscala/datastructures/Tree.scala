package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

case object TreeHelper {
  private def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = t match {
      case a: Branch[A] => b(fold(a.left)(l)(b), fold(a.right)(l)(b))
      case a: Leaf[A] => l(a.value)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case a: Branch[A] => 1 + (depth(a.left) max depth(a.right))
    case a: Leaf[A] => 0
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case a: Branch[A] => new Branch(map(a.left)(f), map(a.right)(f))
    case a: Leaf[A] => Leaf(f(a.value))
  }

  def size[A, B](tree: Tree[A]): Int = {
    tree match {
      case a: Branch[A] => 1 + size(a.left) + size(a.right)
      case a: Leaf[A] => 1
    }
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case a: Branch[Int] => maximum(a.left) max  maximum(a.right)
    case a: Leaf[Int] => a.value
  }
}