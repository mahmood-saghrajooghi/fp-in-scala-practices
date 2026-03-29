sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None    => None
    case Some(x) => Some(f(x))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None    => None
    case Some(x) => f(x)
  }
  def getOrElse[B >: A](defautl: => B): B = this match {
    case None    => defautl
    case Some(x) => x
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case x    => x
  }
  def filter(f: A => Boolean): Option[A] = this match {
    case None    => None
    case Some(x) => if (f(x)) Some(x) else None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e)  => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e)  => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_)  => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap(aa => b.map(bb => f(aa, bb)))
}

object Either {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match {
    case Nil      => Right(Nil)
    case (h :: t) => h.flatMap(hh => sequence(t).map(tt => hh :: tt))
  }

  def traverse[E, A, B](
      es: List[A]
  )(f: (A) => Either[E, B]): Either[E, List[B]] = es match {
    case Nil      => Right(Nil)
    case (h :: t) => f(h).flatMap(hh => traverse(t)(f).map(tt => hh :: tt))
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object MyObject {
  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch { case e: Exception => None }
  }

  def TryViaEither[A](a: => A): Either[Exception, A] = {
    try Right(a)
    catch { case e: Exception => Left(e) }
  }

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(aa => b.map(bb => f(aa, bb)))

  def map3[A, B, C, D](a: Option[A], b: Option[B], c: Option[C])(
      f: (A, B, C) => D
  ): Option[D] =
    a.flatMap(aa => b.flatMap(bb => c.map(cc => f(aa, bb, cc))))

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def meanViaEither(xs: Seq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("The list is empty!")
    else Right(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldRight[Option[List[B]]](Some(Nil))((aa, acc) =>
      map2(f(aa), acc)(_ :: _)
    )
  }

  def sequence[A](xs: List[Option[A]]): Option[List[A]] = xs match {
    case Nil    => Some(Nil)
    case h :: t => h.flatMap(hh => sequence(t).map(tt => hh :: tt))
  }

  def sequenceViaTraverse[A](xs: List[Option[A]]): Option[List[A]] =
    traverse(xs)(x => x)

  def sequenceViaFoldRight[A](xs: List[Option[A]]): Option[List[A]] =
    xs.foldRight[Option[List[A]]](Some(Nil))((acc, oa) => map2(acc, oa)(_ :: _))

  def sequenceViaFoldLeft[A](xs: List[Option[A]]): Option[List[A]] =
    xs.reverse.foldLeft[Option[List[A]]](Some(Nil))((acc, oa) =>
      map2(oa, acc)(_ :: _)
    )

  def saveDiv(x: Int, y: Int): Either[Exception, Int] = {
    try Right(x / y)
    catch { case e: Exception => Left(e) }
  }
  def main(args: Array[String]): Unit = {
    val a: List[Double] = List(2, 1)
    val x = traverse(a)(a => Try(a.toInt))
    val b = List(Some("2"), Some("A"))

    print(meanViaEither(a))
  }
}
