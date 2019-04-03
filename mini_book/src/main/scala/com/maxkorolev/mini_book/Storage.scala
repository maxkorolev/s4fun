package com.maxkorolev.mini_book

import cats.effect.concurrent.MVar
import cats.effect.{Concurrent, Sync}
import cats.syntax.flatMap._
import cats.syntax.functor._

trait Storage[F[_], K, V] {
  def source: F[Map[K, V]]
  def add(id: K, value: V): F[Unit]
  def update(id: K, value: V): F[Unit]
  def delete(id: K): F[Unit]
  def deleteAll(): F[Unit]
}

object Storage {
  def apply[F[_]: Concurrent, K, V]: F[Storage[F, K, V]] = for (mvar <- MVar.of(Map.empty[K, V])) yield Impl(mvar)

  private case class Impl[F[_], K, V](mvar: MVar[F, Map[K, V]])(implicit F: Sync[F]) extends Storage[F, K, V] {

    override def source: F[Map[K, V]] = mvar.take

    override def add(id: K, value: V): F[Unit] =
      mvar.take.flatMap(map => mvar.put(map + (id -> value)))

    override def update(id: K, value: V): F[Unit] =
      mvar.take.flatMap(map => mvar.put(map + (id -> value)))

    override def delete(id: K): F[Unit] =
      mvar.take.flatMap(map => mvar.put(map - id))

    override def deleteAll(): F[Unit] =
      mvar.take.flatMap(_ => mvar.put(Map()))

  }

}
