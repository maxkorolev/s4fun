package com.maxkorolev.recursion

import cats.effect._
import org.scalatest._

import scala.concurrent.ExecutionContext
import scala.annotation.tailrec

class StorageSpec extends FlatSpec with Matchers {

  "Iso" should "ex1" in {

    type A = Either[Boolean, Unit]

    sealed trait B
    case object One extends B
    case object Two extends B
    case object Three extends B

    val iso = new Iso[A, B] {

      override def from(x: A): B = x match {
        case Left(true)  => One
        case Left(false) => Two
        case Right(())   => Three
      }
      override def to(x: B): A = x match {
        case One   => Left(true)
        case Two   => Left(false)
        case Three => Right(())
      }
    }

    (iso.to _ andThen iso.from _)(One) shouldEqual One
    (iso.to _ andThen iso.from _)(Two) shouldEqual Two
    (iso.to _ andThen iso.from _)(Three) shouldEqual Three

  }

  "Iso" should "ex2" in {

    type A = Boolean => Boolean

    sealed trait B
    case object One extends B
    case object Two extends B
    case object Three extends B
    case object Four extends B

    val iso = new Iso[A, B] {

      override def from(x: A): B = (x(true), x(false)) match {
        case (true, true)   => One
        case (false, true)  => Two
        case (true, false)  => Three
        case (false, false) => Four
      }
      override def to(x: B): A = x match {
        case One   => x => if (x) true else true
        case Two   => x => if (x) false else true
        case Three => x => if (x) true else false
        case Four  => x => if (x) false else false
      }
    }

    (iso.to _ andThen iso.from _)(One) shouldEqual One
    (iso.to _ andThen iso.from _)(Two) shouldEqual Two
    (iso.to _ andThen iso.from _)(Three) shouldEqual Three
    (iso.to _ andThen iso.from _)(Four) shouldEqual Four

  }
  "List" should "have foldr" in {

    val lst = Cons("1", Cons("2", Cons("3", Empty)))

    Lst.foldr(lst)("")(_ + _) shouldEqual "321"
  }

  "Expr" should "have foldr" in {

    val exp = Add(Lit(1), Mul(Lit(2), Lit(3)))

    Expr.foldr(exp)(0)(identity)(_ + _)(_ * _) shouldEqual 7
    Expr.foldr(exp)("")(_.toString)((l, r) => s"$l + $r")((l, r) => s"$l * $r") shouldEqual "1 + 2 * 3"

  }

  "Expr" should "have foldr Algebra" in {

    val exp = Add(Lit(1), Mul(Lit(2), Lit(3)))

    ExprF.foldRS[Int, Int](exp) {
      case LitF(t)    => t
      case AddF(l, r) => l + r
      case MulF(l, r) => l * r
    } shouldEqual 7

    ExprF.foldRS[Int, String](exp) {
      case LitF(t)    => t.toString
      case AddF(l, r) => s"$l + $r"
      case MulF(l, r) => s"$l * $r"
    } shouldEqual "1 + 2 * 3"
  }

  "Expr" should "have Recursive instance" in {

    val exp = Add(Lit(1), Mul(Lit(2), Lit(3)))
    val r = new Recursive[Expr, ExprF] {

      override def map[A, B](fa: ExprF[A])(f: A => B): ExprF[B] = fa match {
        case LitF(t)    => LitF(t)
        case AddF(l, r) => AddF(f(l), f(r))
        case MulF(l, r) => MulF(f(l), f(r))
      }

      override def project(z: Expr): ExprF[Expr] = z match {
        case Lit(t)    => LitF(t)
        case Add(l, r) => AddF(l, r)
        case Mul(l, r) => MulF(l, r)
      }

    }

    r.cata[Int](exp) {
      case LitF(t)    => t
      case AddF(l, r) => l + r
      case MulF(l, r) => l * r
    } shouldEqual 7

  }
}
