package stream

sealed trait Stream[+A] {
  def headOption: Option[A] =
    foldRight(None: Option[A])((a, b) => Some(a))

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n-1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n-1)
    case _ => this
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Empty => true
    case Cons(h, _) if !p(h()) => false
    case Cons(_, t) => t().forAll(p)
  }

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) => if (p(a)) Stream.cons(a, b) else b)

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => Stream.cons(a, b))

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => Stream.cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A]) { (a, b) =>
      if (f(a)) Stream.cons(a, b)
      else b
    }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((a, b) => f(a).append(b))

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  def mapViaUnfold[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Cons(a, b) => Some((f(a()), b()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] =
    Stream.unfold((this, n)) {
      case (Cons(a, b), n) if n > 0 => Some((a(), (b(), n-1)))
      case _ => None
    }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(a, b) if (p(a())) => Some((a(), b()))
      case _ => None
    }

  def zipWith[B, C](z: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, z)) {
      case (Cons(a,b), Cons(c,d)) => Some((f(a(),c()), (b(),d())))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    Stream.unfold((this, s2)) {
      case (Cons(a,b), Cons(c,d)) => Some(((Some(a()),Some(c())), (b(),d())))
      case (Empty, Cons(a,b)) => Some(((None,Some(a())), (Empty,b())))
      case (Cons(a,b), Empty) => Some(((Some(a()),None), (b(),Empty)))
      case _ => None
    }

  def startsWith[A](s: Stream[A]): Boolean =
    !zipWith(s)(_ == _).exists(!_)

  def tails: Stream[Stream[A]] =
    Stream.unfold(this) {
      case Empty => None
      case s => Some(s, s.drop(1))
    } append Stream(Stream.empty)
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  val fibs = {
    def go(a: Int, b: Int): Stream[Int] = {
      cons(a, go(b, a+b))
    }
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z).map { case (a, s) => cons(a, unfold(s)(f)) } getOrElse empty[A]
  }

  val onesViaUnfold: Stream[Int] = unfold(1)(_ => Some((1,1)))

  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(_ => Some(a,a))

  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(m => Some(m, m + 1))

  val fibsViaUnfold: Stream[Int] =
    unfold((0,1)) { case (a, b) => Some(a, (b, a + b)) }
}
