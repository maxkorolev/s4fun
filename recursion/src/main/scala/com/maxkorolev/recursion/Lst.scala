package com.maxkorolev.recursion

sealed trait Lst[+T]

case object Empty extends Lst[Nothing]
case class Cons[T](head: T, tail: Lst[T]) extends Lst[T]

object Lst {

  def foldr2[T, Z](lst: Lst[T])(seed: Z)(f: (Z, T) => Z): Z = lst match {
    case Empty            => seed
    case Cons(head, tail) => f(foldr(tail)(seed)(f), head)
  }

  def foldr[T, Z](lst: Lst[T])(seed: Z)(f: (Z, T) => Z): Z = {

    def go(lst: Lst[T], seed: Z): Z = lst match {
      case Empty            => seed
      case Cons(head, tail) => f(go(tail, seed), head)
    }

    go(lst, seed)
  }
}
