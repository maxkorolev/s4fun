package com.maxkorolev.recursion.exp

sealed trait Lst[+T]

case object Empty extends Lst[Nothing]
case class Cons[T](head: T, tail: Lst[T]) extends Lst[T]

object Lst {

  def foldr[T, Z](lst: Lst[T])(init: Z)(f: (Z, T) => Z): Z = lst match {
    case Empty            => init
    case Cons(head, tail) => f(foldr(tail)(init)(f), head)
  }
}
