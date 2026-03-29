sealed trait Stream[+A] {
  def toList: List[A] = this match {
    case Empty      => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _          => z
    }

  def headOption: Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => Some(h())
  }

  def headOptionViaFoldRight: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a));

  def headOptionViaFodlRight: Option[A] = this match {
    case Empty      => None
    case Cons(h, t) => Some(h())
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1  => Stream.cons(h(), t().take(n - 1))
    case Cons(h, t) if n == 1 => Stream.cons(h(), Stream.empty)
    case _                    => Stream.empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _                   => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _                    => Stream.empty
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty)((a, b) =>
      if (p(a)) Stream.cons(a, b) else Stream.empty
    )

  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => (p(h())) || t().exists(p)
    case _          => false
  }

  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) && t().forAll(p)
    case _          => true
  }

  def mapViaFoldRight[B](f: A => B): Stream[B] =
    foldRight(Stream.empty)((a, b) => Stream.cons(f(a), b))

  def filterViaFoldeRight(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty)((a, b) => if (f(a)) Stream.cons(a, b) else b)

  def append[A2 >: A](that: => Stream[A2]): Stream[A2] =
    foldRight(that)((a, b) => Stream.cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty)((a, b) => f(a).append(b))
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
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  // def headOption: Option[A] = this match {
  //   case Empty      => Empty
  //   case Cons(h, t) => Some(h())
  // }
}

def constant[A](a: A): Stream[A] = {
  lazy val const: Stream[A] = Stream.cons(a, const)
  const
}

def from(n: Int): Stream[Int] = {
  lazy val single: Stream[Int] = Stream.cons(n, from(n + 1))
  single
}

def fibs(): Stream[Int] = {
  def go(curr: Int, next: Int): Stream[Int] =
    Stream.cons(curr, go(next, curr + next))
  go(0, 1)

}

object MyObject {

  def main(args: Array[String]): Unit = {
    val x = Stream(1, 33, 69)

    println(x.flatMap(x => Stream(x, 1)).toList)
  }
}
