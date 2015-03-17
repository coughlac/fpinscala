package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

case object TreeHelper {
  def size[A](tree: Tree[A], acc: Int): Int = tree match {
    case a: Branch[A] => size(a.left, size(a.right, acc + 1))
    case a: Leaf[A] => acc + 1
  }
}
