package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

case object TreeHelper {
  def size[A](tree: Tree[A]): Int = size(tree, 0)

  def maximum(tree: Tree[Int]): Int = maximum(tree, 0)

  def depth[A](tree: Tree[A]): Int = tree match {
    case a: Branch[A] => 1 + (depth(a.left) max depth(a.right))
    case a: Leaf[A] => 0
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case a: Branch[A] => new Branch(map(a.left)(f), map(a.right)(f))
    case a: Leaf[A] => Leaf(f(a.value))
  }

  private def size[A](tree: Tree[A], acc: Int): Int = tree match {
    case a: Branch[A] => size(a.left, size(a.right, acc + 1))
    case a: Leaf[A] => acc + 1
  }

  private def maximum(tree: Tree[Int], result: Int): Int = tree match {
    case a: Branch[Int] => maximum(a.left, maximum(a.right, result))
    case a: Leaf[Int] => result.max(a.value)
  }
}
