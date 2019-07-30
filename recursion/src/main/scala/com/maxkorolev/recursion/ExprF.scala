package com.maxkorolev.recursion

sealed trait ExprF[+Z]
case class LitF(t: Int) extends ExprF[Nothing]
case class AddF[Z](l: Z, r: Z) extends ExprF[Z]
case class MulF[Z](l: Z, r: Z) extends ExprF[Z]

object ExprF {

  def foldRS[A, Z](exp: Expr)(algebra: Algebra[ExprF, Z]): Z = exp match {
    case Lit(t)    => algebra(LitF(t))
    case Add(l, r) => algebra(AddF(foldRS(l)(algebra), foldRS(r)(algebra)))
    case Mul(l, r) => algebra(MulF(foldRS(l)(algebra), foldRS(r)(algebra)))
  }

  def project(exp: Expr): ExprF[Expr] = exp match {
    case Lit(t)    => LitF(t)
    case Add(l, r) => AddF(l, r)
    case Mul(l, r) => MulF(l, r)
  }

  def embed(exp: ExprF[Expr]): Expr = exp match {
    case LitF(t)    => Lit(t)
    case AddF(l, r) => Add(l, r)
    case MulF(l, r) => Mul(l, r)
  }

  def map[A, B](exp: ExprF[A])(f: A => B): ExprF[B] = exp match {
    case LitF(t)    => LitF(t)
    case AddF(l, r) => AddF(f(l), f(r))
    case MulF(l, r) => MulF(f(l), f(r))
  }

  def foldRS2[A, Z](exp: Expr)(algebra: Algebra[ExprF, Z]): Z =
    algebra(map(project(exp))(tree => foldRS2(tree)(algebra)))
}
