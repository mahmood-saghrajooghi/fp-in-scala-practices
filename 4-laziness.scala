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

  def takeViaUnfold(n: Int): Stream[A] = unfold(this){ currentStream => currentStream match {
    case Cons(h, t) if n > 1 => Some(h(), t().takeViaUnfold(n -1))
    case Cons(h, t) if n == 1 => Some(h(), Stream.empty[A])
    case Empty => None
  }}


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

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this){ currentStream => currentStream match {
    case Cons(h, t) if p(h()) => Some(h(), t().takeWhileViaUnfold(p))
    case _ => None
  }}

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

  def mapViaUnfold[B](f: A => B): Stream[B] = 
    unfold(this) { currentStream => currentStream match {
      case Cons(h, t) => Some(f(h()), t())
      case Empty => None
    }}

  def filterViaFoldeRight(f: A => Boolean): Stream[A] =
    foldRight(Stream.empty)((a, b) => if (f(a)) Stream.cons(a, b) else b)

  def append[A2 >: A](that: => Stream[A2]): Stream[A2] =
    foldRight(that)((a, b) => Stream.cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty)((a, b) => f(a).append(b))

  def zipWith[B, C](that: Stream[B])(f: (A, B) => C): Stream[C] = unfold(( this, that)){ 
    case (Cons(hh, ht), Cons(th, tt)) => Some(f(hh(), th()), (ht(), tt()))  
    case _ => None
  }

  def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, that)){
    case (Cons(ha, ta), Cons(hb, tb)) => Some(((Some(ha()), Some(hb())), (ta(), tb())))
    case (Cons(ha, ta), Empty) => Some(((Some(ha()), None), (ta(), Empty)))
    case (Empty, Cons(hb, tb)) => Some(((None, Some(hb())), (Empty, tb())))
    case (Empty, Empty) => None
  }

  def startsWith[A](sub: Stream[A]): Boolean = (this, sub) match {
    case (Empty, Empty) => true
    case (Empty, Cons(_, _)) => false
    case (Cons(ha, ta), Empty) => true
    case (Cons(ha, ta), Cons(hb, tb)) if(ha() == hb()) => ta().startsWith(tb())
    case _ => false
  }

  def startWith1[A](sub: Stream[A]): Boolean = this.zipAll(sub).takeWhile(_(1).isDefined).forAll((a, b) => a == b)

  def tails: Stream[Stream[A]] = unfold(this){
    case s @ Cons(h, t) => Some(s, t())
    case Empty => None
  }.append(Stream(Stream.empty))

  def hasSubsequence[A](sub: Stream[A]): Boolean = tails.exists(_.startWith1(sub))

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

def unfold[A, S](state: S)(f: S => Option[(A, S)]): Stream[A] = f(state) match {
  case Some((h, s)) => Stream.cons(h, unfold(s)(f))
  case None => Stream.empty
}

def constant[A](a: A): Stream[A] = {
  lazy val const: Stream[A] = Stream.cons(a, const)
  const
}

def constantFromUnfold[A](a: A): Stream[A] = unfold(a)((a) => Some(a, a))

def from(n: Int): Stream[Int] = {
  lazy val single: Stream[Int] = Stream.cons(n, from(n + 1))
  single
}

def fromViaUnfold(n: Int): Stream[Int] =unfold(n)((s) => Some((s, s+1)))

def fibs(): Stream[Int] = {
  def go(curr: Int, next: Int): Stream[Int] =
    Stream.cons(curr, go(next, curr + next))
  go(0, 1)
}

def fibsViaUnfold: Stream[Int] = unfold((0, 1)):
  case (current, next) => Some((current), (next, current + next))

def onesViaUnfoid: Stream[Int] = unfold(())(_ => Some(1, ()))

object MyObject {

  def main(args: Array[String]): Unit = {
    val x = Stream(1, 33, 69)
    val y = Stream(1, 33, 69, 85)
    val z = Stream(33, 69)

    println(z.hasSubsequence(y))
  }
}
