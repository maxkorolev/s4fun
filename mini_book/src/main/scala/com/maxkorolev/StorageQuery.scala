package com.maxkorolev

import cats._
import cats.syntax.functor._

trait StorageQuery[F[_], K, V] {
  def getById(id: K): F[Option[V]]
  def getSorted: F[List[V]]
}

object StorageQuery {

  def apply[F[_]: Monad, K: Ordering, V: Ordering](storage: Storage[F, K, V]): F[StorageQuery[F, K, V]] =
    Applicative[F].pure(Impl(storage))

  private case class Impl[F[_]: Monad, K: Ordering, V: Ordering](storage: Storage[F, K, V]) extends StorageQuery[F, K, V] {

    override def getById(id: K): F[Option[V]] = storage.source.map(_.get(id))

    override def getSorted: F[List[V]] =
      storage.source.map(_.toList.sortBy(_._1).map(_._2).sorted)
  }
}

