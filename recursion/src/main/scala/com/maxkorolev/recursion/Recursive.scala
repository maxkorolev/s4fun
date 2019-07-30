package com.maxkorolev.recursion

import cats.Functor

trait Recursive[T, F[_]] extends Functor[F] {

  def project(z: T): F[T]
  def cata[Z](exp: T)(algebra: Algebra[F, Z]): Z =
    algebra(map(project(exp))(tree => cata(tree)(algebra)))
}
