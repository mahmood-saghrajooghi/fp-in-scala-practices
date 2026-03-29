package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def foldRight[A, B](as: List[A], y: B)(f: (A, B) => B): B = as match {
    case Nil         => y
    case Cons(x, xs) => f(x, foldRight(xs, y)(f))
  }

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], y: B)(f: (B, A) => B): B = as match {
    case Nil        => y
    case Cons(h, t) => foldLeft(t, f(y, h))(f)
  }

  def foldRightViaLeft[A, B](as: List[A], y: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), y)((acc, a) => f(a, acc))

  def sum(ns: List[Int]): Int = {
    foldRightViaLeft(ns, 0)((x, y) => x + y)
  }

  def product(ds: List[Double]): Double = {
    foldRight(ds, 1.0)(_ * _)
  }

  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil)((acc, h) => Cons(h, acc))
  }

  def apply[A](as: A*): List[A] =
    as.foldRight(Nil: List[A])((a, acc) => Cons(a, acc))

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil         => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](xs: List[A], a: A): List[A] = xs match {
    case Nil         => List(a)
    case Cons(x, xs) => Cons(a, xs)
  }

  def drop[A](xs: List[A], n: Int): List[A] = xs match {
    case Nil         => Nil
    case Cons(x, xs) =>
      n match {
        case 0 => Cons(x, xs)
        case _ => List.drop(xs, n - 1)
      }
  }

  def dropWhile[A](xs: List[A], f: A => Boolean): List[A] = xs match {
    case Nil         => Nil
    case Cons(x, xs) => {
      if (f(x)) dropWhile(xs, f)
      else Cons(x, xs)
    }
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil        => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def concat[A](xs: List[List[A]]): List[A] = xs match {
    case Nil        => Nil
    case Cons(h, t) => append(h, List.concat(t))
  }

  def concat1[A](xs: List[List[A]]): List[A] =
    foldLeft(xs, Nil: List[A])((acc, a) => append(acc, a))

  def init[A](xs: List[A]): List[A] = xs match {
    case Nil          => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t)   => Cons(h, init(t))

  }

  def length[A](as: List[A]): Int = {
    foldLeft(as, 0)((acc, _) => acc + 1)
  }

  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil        => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil        => Nil
    case Cons(h, t) => if (f(h)) Cons(h, filter(t)(f)) else filter(t)(f)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil        => Nil
    case Cons(h, t) => append(f(h), flatMap(t)(f))
  }

  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if (f(x)) List(x) else Nil)

  def zipWith[A](a: List[A], b: List[A])(f: (A, A) => A): List[A] =
    (a, b) match {
      case (Cons(ha, ta), Cons(hb, tb)) => Cons(f(ha, hb), zipWith(ta, tb)(f))
      case _                            => Nil
    }

  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil)                                   => true
    case (Cons(h1, t1), Cons(h2, t2)) if (h1 == h2) =>
      startsWith(t1, t2)
    case (_) => false
  }

  def hasSubsequent[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil                       => false
    case _ if startsWith(sup, sub) => true
    case Cons(_, t)                => hasSubsequent(t, sub)
  }
}

// a comment

/* another comment */

object MyModule {

  def abs(n: Int): Int = {
    if (n < 0) -n
    else n
  }

  def factorial(n: Int): Int = {
    def go(n: Int, acc: Int): Int = {
      if (n <= 0) acc
      else go(n - 1, n * acc)
    }

    go(n, 1)
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(i: Int, prev: Int, curr: Int): Int = {
      if (i >= n) prev
      else loop(i + 1, curr, prev + curr)
    }

    if (n < 0) throw new IllegalArgumentException("n must be >= 0")
    else loop(0, 0, 1)
  }

  def addOne(as: List[Int]): List[Int] = as match {
    case Nil        => Nil
    case Cons(h, t) => Cons(h + 1, addOne(t))
  }

  def toString(as: List[Double]): List[String] = as match {
    case Nil        => Nil
    case Cons(h, t) => Cons(h.toString(), toString(t))
  }

  def addLists(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
    case (Cons(ha, ta), Cons(hb, tb)) => Cons(ha + hb, addLists(ta, tb))
    case _                            => Nil
  }

  def main(args: Array[String]): Unit = {
    val x = List(1, 2, 3)
    val a = List(1.0, 2.0, 3.0)
    val y = List(2, 2)
    val z = List(x, y)

    println(List.hasSubsequent(x, y))
  }
}
