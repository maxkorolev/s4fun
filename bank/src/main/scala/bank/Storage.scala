package bank

import cats.effect.concurrent.Ref
import cats.effect.{Concurrent, Sync}
import cats.syntax.flatMap._
import cats.syntax.functor._

trait Storage[F[_], K, V] {
  def source: F[Map[K, V]]
  def get(id: K): F[Option[V]]
  def add(id: K, value: V): F[Unit]
  def update(id: K, value: V): F[Unit]
  def updateValue(id: K)(f: V => V): F[Unit]
  def delete(id: K): F[Unit]
  def deleteAll(): F[Unit]
}

object Storage {
  def apply[F[_]: Concurrent, K, V]: F[Storage[F, K, V]] =
    for (ref <- Ref.of(Map.empty[K, V])) yield Impl(ref)

  private case class Impl[F[_], K, V](ref: Ref[F, Map[K, V]])(
      implicit F: Sync[F]
  ) extends Storage[F, K, V] {

    override def source: F[Map[K, V]] = ref.get

    override def get(id: K): F[Option[V]] = ref.get.map(_.get(id))

    override def add(id: K, value: V): F[Unit] =
      ref.update(_ + (id -> value))

    override def update(id: K, value: V): F[Unit] =
      ref.update(_ + (id -> value))

    override def updateValue(id: K)(f: V => V): F[Unit] =
      ref.update(map => map + (id -> f(map(id))))

    override def delete(id: K): F[Unit] =
      ref.update(_ - id)

    override def deleteAll(): F[Unit] =
      ref.update(_ => Map())
  }

}
