package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {
  // exercise 3.25 -- counts the number of nodes (leaves and branches)
  //                  in a tree
  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => 1 + size(l) + size(r)
    }

  // exerise 3.26 -- returns the maximum element in a Tree[Int]
  def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(x) => x
      case Branch(l, r) => maximum(l).max(maximum(r))
    }

  // exercise 3.27 -- returns the maximum path length from the
  //                  root of a tree to any leaf
  def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 0
      case Branch(l, r) => 1 + depth(l).max(depth(r))
    }

  // exericse 3.28 -- map - modifies each element in a tree with a
  //                  given function
  def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

  // exercise 3.29 -- generalize size, maximum, depth, map writing a new
  //                  function fold that abstracts over their similarities

  def fold[A, B](t: Tree[A])(lf: A => B)(bf: (B, B) => B): B =
    t match {
      case Leaf(a) => lf(a)
      case Branch(l, r) => bf(fold(l)(lf)(bf), fold(r)(lf)(bf))
    }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(x => 1)((l, r) => 1 + l + r)

  def maximumViaFold[A](t: Tree[Int]): Int =
    fold(t)(x => x)((l, r) => l.max(r))

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(x => 0)((l, r) => 1 + l.max(r))

  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(a => Leaf(f(a)): Tree[B])((l, r) => Branch(l, r))
  
}
