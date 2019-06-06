package com.maxkorolev.mini_book

import cats._
import cats.syntax.functor._

trait StorageQuery[F[_], K, V] {
  def getById(id: K): F[Option[V]]
  def getSorted: F[List[(K, V)]]
}

object StorageQuery {

  def apply[F[_]: Monad, K: Ordering, V: Ordering](storage: Storage[F, K, V]): F[StorageQuery[F, K, V]] =
    Applicative[F].pure(Impl(storage))

  private case class Impl[F[_]: Monad, K: Ordering, V: Ordering](storage: Storage[F, K, V]) extends StorageQuery[F, K, V] {

    override def getById(id: K): F[Option[V]] =
      storage.source.map(_.get(id))

    override def getSorted: F[List[(K, V)]] =
      storage.source.map(_.toList.sorted)
  }
}

