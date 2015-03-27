package fpinscala.datastructures

sealed trait Tree[+A] {
  def fold[B](l: A => B)(b: (B, B) => B): B
}

case class Leaf[A](value: A) extends Tree[A] {
  def fold[B](l: (A) => B)(b: (B, B) => B): B = l(value)
}

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
  def fold[B](l: (A) => B)(b: (B, B) => B): B = b(left.fold(l)(b), right.fold(l)(b))
}

case object TreeHelper {
  private def fold[A, B](t: Tree[A])(l: A => B)(b: (B, B) => B): B = t match {
      case a: Branch[A] => b(fold(a.left)(l)(b), fold(a.right)(l)(b))
      case a: Leaf[A] => l(a.value)
  }

  def depth[A](tree: Tree[A]): Int = fold[A, Int](tree)(a => 0)((l, r) => 1 + (l max r))

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold[A, Tree[B]](tree)(a => Leaf(f(a)))(Branch(_, _))

  def size[A](tree: Tree[A]): Int = tree.fold[Int](_ => 1)((l, r) => l + r + 1)
  //fold[A, Int](tree)(_ => 1)((l, r) => l + r + 1)

  def maximum(tree: Tree[Int]): Int = fold[Int, Int](tree)(a => a)(_ max _)
}