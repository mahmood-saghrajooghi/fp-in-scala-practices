package fpinscala.datastructures

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def foldRight[A, B](as: List[A], y: B)(f: (A, B) => B): B = as match {
    case Nil => y
    case Cons(x,xs) => f(x, foldRight(xs, y)(f))
  }

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], y: B)(f: (B, A) => B): B = as match {
    case Nil => y
    case Cons(h,t) => foldLeft(t, f(y, h))(f)
  }

  def sum(ns: List[Int]): Int = {
    foldLeft(ns, 0)((x, y) => x + y)
  }

  def product(ds: List[Double]): Double = {
    foldRight(ds, 1.0)(_ * _)
  }

  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, Nil)((acc, h) => Cons(h, acc))
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail*))

  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(x, xs) => xs
  }

  def setHead[A](xs: List[A], a: A): List[A] = xs match {
    case Nil => List(a)
    case Cons(x, xs) => Cons(a, xs)
  }

  def drop[A](xs: List[A], n: Int): List[A] = xs match {
    case Nil => Nil
    case Cons(x, xs) => n match {
      case 0 => Cons(x, xs)
      case _ => List.drop(xs, n-1)
    }
  }

  def dropWhile[A](xs: List[A], f: A => Boolean): List[A] = xs match {
    case Nil => Nil
    case Cons(x, xs) => {
      if(f(x)) dropWhile(xs, f)
      else Cons(x, xs)
    }
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  def init[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(_, Nil)=> Nil
    case Cons(h, t) => Cons(h, init(t))

  }

  def length[A](as: List[A]): Int = {
    foldLeft(as, 0)((acc,_) => acc + 1)
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
      if (n<=0) acc
      else go(n-1, n*acc)
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

  def main(args: Array[String]): Unit = {
    val x = List(1, 2, 3)
    val y = List(1.0, 2.0, 3.0)

    println(List.length(x))
    println(List.sum(x))
    println(List.product(y))
    println(List.reverse(x))
  }
}
