package com.maxkorolev

import cats.effect.concurrent.MVar
import cats.effect.syntax.bracket._
import cats.effect.{Concurrent, ExitCase, Sync}
import cats.instances.either._
import cats.instances.list._
import cats.instances.option._
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.flatMap._

trait Storage[F[_], K, V] {
  def getAll: F[List[V]]
  def get(id: K): F[Option[V]]
  def add(id: K, value: V): F[Unit]
  def update(id: K, value: V): F[Unit]
  def delete(id: K): F[Unit]
  def deleteAll(): F[Unit]
}

object Storage {
  def apply[F[_]: Concurrent, K, V]: F[Storage[F, K, V]] = for (mvar <- MVar.of(Map.empty[K, V])) yield Impl(mvar)

  private case class Impl[F[_], K, V](mvar: MVar[F, Map[K, V]])(implicit F: Sync[F]) extends Storage[F, K, V] {
    override def getAll: F[List[V]] = mvar.read.map(_.values.toList)

    override def get(id: K): F[Option[V]] = mvar.read.map(_.get(id))

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
