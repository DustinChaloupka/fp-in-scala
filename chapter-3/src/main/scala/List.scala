sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x,xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // 3.1
  def threeOne(): Boolean = {
    val x = List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) =>x+y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }

    x == 3
  }

  // 3.2
  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil // Book uses sys.error("tail of empty list"), probably for a better reason: https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/datastructures/List.scala#L58
    case Cons(_, ys) => ys
  }

  // 3.3
  def setHead[A](newHead: A, xs: List[A]): List[A] = xs match {
    case Nil => Nil // or case Nil => Cons(newHead, Nil) depending how the exercise is interpreted, though the book answers through an error instead: https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/datastructures/List.scala#L72
    case Cons(_, tail) => Cons(newHead, tail)
  }

  // 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  // 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  // 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) =>  Cons(h, init(t))
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  // 3.7
  // No if we are not allowed to change the `foldRight` function itself?
  // Otherwise...
  // def foldRight[A,B](as: List[A], z: B)(shortCircuitValue: A, check: A => Boolean)(f: (A, B) => B): B = as match {
  //   case Nil => z
  //   case Cons(x, xs) =>
  //     if (check(x)) f(shortCircuitValue, z)
  //     else f(x, foldRight(xs, z)(shortCircuitValue, check)(f))
  // }
  // But this sort of thing might not work be logical depending on what types are being used

  // 3.8
  // foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
  // Cons(1, foldRight(List(2,3), Nil:List[Int])(Cons(_,_)))
  // Cons(1, Cons(2, foldRight(List(3), Nil:List[Int])(Cons(_,_))))
  // Cons(1, Cons(2, Cons(3, foldRight(Nil, Nil:List[Int])(Cons(_,_)))))
  // Cons(1, Cons(2, Cons(3, Nil)))
  // List(1,2,3)

  // So it's the identity function for List.

  // 3.9
  def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_,count) => count + 1)
  }

  // 3.10
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  // 3.11
  def sumViaFoldLeft(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  def productViaFoldLeft(ns: List[Double]) =
    foldLeft(ns, 1.0)(_ * _)

  def lengthViaFoldLeft[A](l: List[A]): Int =
    foldLeft(l, 0)((count,_) => count + 1)

  // 3.12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil:List[A])((acc,h) => Cons(h, acc))

  // 3.13
  def foldRightTailRecursive[A,B](l: List[A], z: B)(f: (A,B) => B): B =
    foldLeft(reverse(l),z)((acc,h) => f(h,acc))

  // 3.14
  def append[A](l: List[A], z: List[A]): List[A] =
    foldRight(l,z)(Cons(_,_))

  // 3.15
  def concatenate[A](ll: List[List[A]]): List[A] =
    foldRight(ll,List[A]())((h,acc) => append(h, acc))

  // 3.16
  def addOneToEach(l: List[Int]): List[Int] =
    foldRight(l, List[Int]())((h, acc) => Cons(h+1, acc))

  // 3.17
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, List[String]())((h, acc) => Cons(h.toString, acc))

  // 3.18
  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as,List[B]())((h,acc) => Cons(f(h),acc))

  // 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as,List[A]()) { (h, acc) =>
      if (f(h)) Cons(h, acc)
      else acc
    }

  // 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    concatenate(map(as)(f))

  // 3.21
  def filterWithFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  // 3.22
  def zipAdd(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a, ta), Cons(b, tb)) => Cons(a+b, zipAdd(ta,tb))
  }

  // 3.23
  def zipWith[A](as: List[A], bs: List[A])(f: (A,A) => A): List[A] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a, ta), Cons(b, tb)) => Cons(f(a,b), zipWith(ta,tb)(f))
  }

  // 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    @annotation.tailrec
    def go(sup: List[A], sub: List[A], startedSeq: Boolean): Boolean = (sup, sub) match {
      case (_, Nil) if startedSeq => true
      case (Nil, Cons(_,_)) => false
      case (Cons(a, ta), Cons(b, tb)) if (a == b) => go(ta, tb, true)
      case (Cons(_, t), l) if startedSeq => false
      case (Cons(_, t), l) => go(t,l,false)
    }
    go(sup, sub, false)
  }
}
