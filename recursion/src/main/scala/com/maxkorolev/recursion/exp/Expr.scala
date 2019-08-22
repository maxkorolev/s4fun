package com.maxkorolev.recursion.exp

sealed trait Expr
case class Lit(t: Int) extends Expr
case class Add(l: Expr, r: Expr) extends Expr
case class Mul(l: Expr, r: Expr) extends Expr

object Expr {

  def foldr[Z](exp: Expr)(
      seed: Z
  )(lit: Int => Z)(add: (Z, Z) => Z)(mul: (Z, Z) => Z): Z = exp match {
    case Lit(t) => lit(t)
    case Add(l, r) =>
      add(foldr(l)(seed)(lit)(add)(mul), foldr(r)(seed)(lit)(add)(mul))
    case Mul(l, r) =>
      mul(foldr(l)(seed)(lit)(add)(mul), foldr(r)(seed)(lit)(add)(mul))
  }
}
