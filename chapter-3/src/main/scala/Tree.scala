sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(tl, tr) => size(tl) + size(tr) + 1
  }

  def maximum(t: Tree[Int]): Int = t match {
    case Leaf(a) => a
    case Branch(tl, tr) => maximum(tl) max maximum(tr)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(tl, tr) => (depth(tl) max depth(tr)) + 1
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(a) => Leaf(f(a))
    case Branch(tl, tr) => Branch(map(tl)(f), map(tr)(f))
  }

  def fold[A,B](t: Tree[A])(z: A => B)(f: (B,B) => B): B = t match {
    case Leaf(a) => z(a)
    case Branch(tl, tr) => f(fold(tl)(z)(f), fold(tr)(z)(f))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((a,b) => a + b + 1)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(a => a)((a,b) => a max b)

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 0)((a,b) => (a max b) + 1)

  def mapViaFold[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])((a,b) => Branch(a, b))
}
