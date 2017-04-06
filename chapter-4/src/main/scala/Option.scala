sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }


  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case s: Some[_] => s
    case None => ob
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => Some(a)
    case _ => None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
    case (None, _) => None
    case (_, None) => None
    case (Some(x), Some(y)) => Some(f(x, y))
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight(Some(Nil): Option[List[A]])((acc,x) => x.flatMap(y => acc.map(_ +: y)))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    sequence(a.map(f(_)))
}
