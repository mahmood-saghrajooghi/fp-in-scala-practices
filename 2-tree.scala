sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def fold[A, B](t: Tree[A])(f: A => B, g: (B, B) => B): B = t match {
    case Leaf(v: A)                     => f(v)
    case Branch(l: Tree[A], r: Tree[A]) => g(fold(l)(f, g), fold(r)(f, g))
  }

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def max(t: Tree[Int]): Int = t match {
    case Leaf(v)      => v
    case Branch(l, r) => max(l).max(max(r))
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_)      => 1
    case Branch(l, r) => 1 + math.max(depth(l), depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(v)      => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }
}

object MyModule {
  def main(args: Array[String]): Unit = {
    var x = Branch(
      Branch(
        Branch(Leaf(1), Leaf(1)),
        Leaf(3)
      ),
      Leaf(2)
    )

    println(Tree.fold(x)(x => 1, (y, y1) => y + y1))
  }
}
