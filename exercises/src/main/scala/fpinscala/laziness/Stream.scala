package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // exercise 5.1 -- convert stream to a list
  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case Empty => List()
  }

  // exercise 5.2 -- return first n elements of stream
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case Empty => empty
  }

  // exercise 5.2 -- drop first n elements of stream
  def drop(n: Int): Stream[A] = {
    if (n <= 0) this
    else this match {
      case Cons(h, t) => t().drop(n - 1)
      case Empty => this
    }
  }

  // exercise 5.3 -- take first elements that satisfy a predicate
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) => if (p(h())) cons(h(), t().takeWhile(p)) else Empty
    case Empty => this
  }

  // exercise 5.4 -- check all elements match a given predicate
  def forAll(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => if (p(h())) t().forAll(p) else false
    case Empty => true
  }

  // exercise 5.5 -- foldRight to implement takeWhile
  def takeWhileViaFR(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
      if (p(h)) cons(h, t)
      else empty)

  // exercise 5.6 -- headOption using foldRight
  def headOption: Option[A] = sys.error("todo")

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def mapViaFR[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filterViaFR(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

  def appendViaFR[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMapViaFR[B>:A](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).appendViaFR(t))

  // exercise 5.13 -- map, take, takeWhile, zipWith, zipAll using unfold
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  def takeViaUnfold(n: Int): Stream[A] = {
    unfold((this, n)) {
      case (Cons(h, t), 1) => Some((h(), (empty, 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _ => None
    }
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = {
    unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }
  }

  def zipWith[B,C](s2: Stream[B])(f: (A, B) => C): Stream[C] = {
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }
  }

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = {
      unfold((this, s2)) {
        case (Empty, Empty) => None
        case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]), (t(), empty[B]))
        case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())), (empty[A], t()))
        case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())), (t1(), t2()))
      }
    }

  def zipAll[B, C](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
      unfold((this, s2)) {
        case (Empty, Empty) => None
        case (Cons(h, t), Empty) => Some((Some(h()), Option.empty[B]), (t(), empty[B]))
        case (Empty, Cons(h, t)) => Some((Option.empty[A], Some(h())), (empty[A], t()))
        case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      }
    }

  // exercise 5.14 -- check if input stream is a prefix of this stream
  def startsWith[B](s: Stream[B]): Boolean = {
    zipAll(s).takeWhile(!_._2.isEmpty).forAll {
      case (h1, h2) => h1 == h2
    }
  }

  def tails: Stream[Stream[A]] = {
    unfold(this) {
      case Empty => None
      case s => Some((s, s drop 1))
    } appendViaFR Stream(empty)
  }

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
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  // exercise 5.8 -- generalize ones for a given value
  def constant(n: Int): Stream[Int] = Stream.cons(n, constant(n))

  // exercise 5.9 -- generate an infinite stream of integers
  //                 starting from n, then n+1, n+2, ...
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  // exercise 5.10 -- generate infinite stream of fibonacci numbers
  def fibs(): Stream[Int] = {
    def go(fib0: Int, fib1: Int): Stream[Int] =
      Stream.cons(fib0, go(fib1, fib0 + fib1))
    go(0, 1)
  }

  // exercise 5.11 -- takes an initial state and a function for producing both
  //                  the next state and next value in the generated stream
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((h, s)) => cons(h, unfold(s)(f))
      case None => empty
    }
  }
  // exercise 5.12 -- write fibs, from, constant, and ones in terms of unfold
  val onesViaUnfold = unfold(1)(_ => Some(1, 1))

  def constantViaUnfold(n: Int): Stream[Int] = {
    unfold(n)(_ => Some(n, n))
  }

  def fromViaUnfold(n: Int): Stream[Int] = {
    unfold(n)(x => Some(x, x + 1))
  }

  def fibsViaUnfold(): Stream[Int] = {
    unfold((0, 1)) {
      case (f0, f1) => Some((f0, (f1, f0 + f1)))
    }
  }

}
