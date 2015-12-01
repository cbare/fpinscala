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


  def tail[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, xs) => xs
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Nil => Cons(h, Nil)
      case Cons(x, xs) => Cons(h, xs)
    }

  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case Cons(x,xs) if n>0 => drop(xs, n-1)
      case _ => l
    }

  def dropWhile[A](l: List[A])(p: A => Boolean): List[A] =
    l match {
      case Cons(x, xs) if p(x) => dropWhile(xs)(p)
      case _ => l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, Nil) =>
        Nil
      case Cons(x, xs) =>
        Cons(x, init(xs))
    }

  // def length[A](l: List[A]): Int =
  //   l match {
  //     case Nil => 0
  //     case Cons(x, xs) => 1 + length(xs)
  //   }
  def length[A](l: List[A]): Int =
    foldLeft(l, 0)((acc,_) => acc + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x,xs) => foldLeft(xs, f(z, x))(f)
    }

  // 3.12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil:List[A])((xs,x)=>Cons(x,xs))

  // 3.13
  def foldRightInTermsOfFoldLeft[A,B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((xs,x)=>f(x,xs))

  def foldLeftinTermsOfFoldRight[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z)((x,xs)=>f(xs,x))

  // 3.14
  def append2[A](xs: List[A], ys: List[A]): List[A] =
    foldRight(xs, ys)(Cons.apply)

  // 3.15
  def concat[A](ls: List[List[A]]): List[A] =
    foldRight(ls, Nil:List[A])((l,acc)=>foldRight(l,acc)(Cons.apply))

  // 3.16
  def add1(xs: List[Int]): List[Int] =
    foldRight(xs, Nil:List[Int])((x,acc)=>Cons(x+1, acc))

  // 3.17
  def doublesToString(xs: List[Double]): List[String] =
    foldRight(xs, Nil:List[String])((x,acc)=>Cons(x.toString(),acc))

  // 3.18
  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((h,t)=>Cons(f(h), t))

  // 3.19
  def filter[A](l: List[A])(p: A => Boolean): List[A] =
    foldRight(l, Nil:List[A])((h,t)=>if (p(h)) Cons(h, t) else t)

  // 3.20
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  // 3.21
  def filterViaFlatMap[A](l: List[A])(p: A => Boolean): List[A] =
    flatMap(l)( (x) => if (p(x)) List(x) else Nil )

  // 3.22
  def addElementwise(l1: List[Int], l2: List[Int]): List[Int] =
    (l1,l2) match {
      case (Cons(x,xs), Cons(y,ys)) => Cons(x+y, addElementwise(xs,ys))
      case (Cons(x,xs), Nil) => Cons(x, addElementwise(xs,Nil))
      case (Nil, Cons(y,ys)) => Cons(y, addElementwise(Nil,ys))
      case (Nil, Nil) => Nil
    }

  // 3.23
  def zipWith[A,B](l1: List[A], l2: List[A])(f: (A,A) => B): List[B] =
    (l1,l2) match {
      case (Cons(x,xs), Cons(y,ys)) => Cons(f(x,y), zipWith(xs,ys)(f))
      case (Nil, _) => Nil
      case (_, Nil) => Nil
    }

  // 3.24
  @annotation.tailrec
  def startsWith[A](sup: List[A], pre: List[A]): Boolean =
    (sup, pre) match {
      case (Cons(x,xs), Cons(y,ys)) => if (x==y) startsWith(xs,ys) else false
      case (Cons(x,xs), Nil) => true
      case (Nil,Nil) => true
      case _ => false
    }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    (sup,sub) match {
      case (_,Nil) => true
      case (Cons(x,xs),_) => if (startsWith(sup, sub)) true else hasSubsequence(xs,sub)
      case _ => false
    }

}
