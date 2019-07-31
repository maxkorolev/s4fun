package com.maxkorolev.recursion

sealed trait ExprF[+Z]
case class LitF(t: Int) extends ExprF[Nothing]
case class AddF[Z](l: Z, r: Z) extends ExprF[Z]
case class MulF[Z](l: Z, r: Z) extends ExprF[Z]

object ExprF {

  def foldRS[Z, A](exp: Expr)(f: Algebra[ExprF, A]): A = exp match {
    case Lit(t)    => f(LitF(t))
    case Add(l, r) => f(AddF(foldRS(l)(f), foldRS(r)(f)))
    case Mul(l, r) => f(MulF(foldRS(l)(f), foldRS(r)(f)))
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

  def foldRS2[Z, A](exp: Expr)(f: Algebra[ExprF, A]): A =
    f(map(project(exp))(v => foldRS2(v)(f)))

}
