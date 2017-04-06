sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(a) => Right(f(a))
    case l: Left[_] => l
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Right(a) => f(a)
    case l: Left[_] => l
  }

  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case r: Right[_] => r
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
    case (Left(l), _) => Left(l)
    case (_, Left(l)) => Left(l)
    case (Right(a), Right(b)) => Right(f(a, b))
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldRight(Right(Nil): Either[E, List[A]])((acc,x) => x.flatMap(y => acc.map(z => z +: y)))

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    sequence(as.map(f(_)))
}
