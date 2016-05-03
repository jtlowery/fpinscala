package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // exercise 3.1 should be 3
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // exercise 3.2
  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("tail of empty list")
      case Cons(_, t) => t
    }

  // exercise 3.3
  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => sys.error("setting head of empty list")
      case Cons(_, t) => Cons(h, t)
    }

  // exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => sys.error("dropping elements of empty list")
      case Cons(_, t) => drop(t, n - 1)
    }

  // exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => sys.error("dropping elements of empty list")
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }

  // exercise 3.6
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => sys.error("dropping tail of empty list")
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }

  // exercise 3.7
  // can product implemented using foldRight immediately halt the recursion
  // and return 0.0 if it encounters a 0.0
  // A: This isn't possible because foldRight must traverse the list to the end

  // exercise 3.8
  // see what happens when you call Nil and Cons themselves to foldRight
  // foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
  // what do you think this says about the relationship between
  // foldRight and the data constructors of the List?
  // A: Get back the original list.
  // foldRight replaces 'Nil' constructor of the list with the 'z' argument
  // which is 'Nil' in this case and replaces the 'Cons' constructor
  // replaces the 'Cons' constructor with 'f' which is 'Cons' here

  // exercise 3.9
  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, y) => 1 + y)

  // exercise 3.10
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(h, t) => foldLeft(t, f(z, h))(f)
    }

  // exercise 3.11 sum in terms of foldLeft
  def sumFoldLeft(l: List[Int]) =
    foldLeft(l, 0)((x, y) => x + y)

  // exercise 3.11 product in terms of foldLeft
  def productFoldLeft(l: List[Double]) =
    foldLeft(l, 1.0)((x, y) => x * y)

  // exercise 3.11 length in terms of foldLeft
  def lengthFoldLeft[A](l: List[A]) =
    foldLeft(l, 0)((x, _) => x + 1)

  // exercise 3.12 reverse an input list
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((h, t) => Cons(t, h))

  // exercise 3.13 -- foldLeft in terms of foldRight
  /*def foldLeftViafoldRight[A,B](l: List[A], z: B)(f: (B, A) => B): B =
  */
  //def foldRightViaFoldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B =
  //  foldLeft(reverse(l, z)

  /*def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }*/

  // exercise 3.14 append in terms of foldRight or foldLeft
  def appendFoldRight[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((h, t) => Cons(h, t))
  def appendFoldLeft[A](a1: List[A], a2: List[A]): List[A] =
    foldLeft(reverse(a1), a2)((h, t) => Cons(t, h))

  // exercise 3.15 -- concatenate a list of lists into a single list
  //def concatLists[A](l: List[List[A]]): List[A] =

  // exercise 3.16 -- add one to each element of an int list
  //def addOne[Int](l: List[Int]): List[Int] =


  def map[A,B](l: List[A])(f: A => B): List[B] = sys.error("todo")

}
