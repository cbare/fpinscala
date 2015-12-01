package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // 3.25
  def size[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(left,right) => 1 + size(left) + size(right)
    }

  // 3.26
  def maximum(t: Tree[Int]): Int =
    t match {
      case Leaf(value) => value
      case Branch(left,right) => maximum(left) max maximum(right)
    }

  // 3.27
  def depth[A](t: Tree[A]): Int =
    t match {
      case Leaf(_) => 1
      case Branch(left,right) => (depth(left) max depth(right)) + 1
    }

  // 3.28
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] =
    t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(left,right) => Branch(map(left)(f), map(right)(f))
    }

  // 3.29
  def fold[A,B](t: Tree[A])(f: A => B)(combine: (B,B)=>B): B =
    t match {
      case Leaf(a) => f(a)
      case Branch(left,right) => combine(fold(left)(f)(combine), fold(right)(f)(combine))
    }

  def size2[A](t: Tree[A]): Int =
    fold(t)((a)=>1)((r,l)=>1+r+l)

  def max2(t: Tree[Int]): Int =
    fold(t)((a)=>a)((x,y)=> x max y)

  def depth2[A](t: Tree[A]): Int =
    fold(t)((a)=>1)((x,y)=> (x max y) + 1)

  def map2[A,B](t: Tree[A])(f: A=>B): Tree[B] = {
    fold(t)(a => Leaf(f(a)): Tree[B])(Branch.apply)
  }
}