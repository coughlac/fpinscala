package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

case object TreeHelper {
  def size[A](tree: Tree[A]): Int = size(tree, 0)

  private def size[A](tree: Tree[A], acc: Int): Int = tree match {
    case a: Branch[A] => size(a.left, size(a.right, acc + 1))
    case a: Leaf[A] => acc + 1
  }

  def maximum(tree: Tree[Int]): Int = maximum(tree, 0)

  private def maximum(tree: Tree[Int], result: Int): Int = tree match {
    case a: Branch[Int] => maximum(a.left, maximum(a.right, result))
    case a: Leaf[Int] => result.max(a.value)
  }
}
