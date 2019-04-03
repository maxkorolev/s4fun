package com.maxkorolev.mini_book

import java.time.Instant

import cats._
import cats.data.OptionT
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.option._
import cats.instances.option._
import cats.effect._

trait QuoteService[F[_]] {

  def newQuote(id: QuoteID, price: QuotePrice, volume: QuoteVolume): F[Unit]
  def updateQuote(id: QuoteID, price: QuotePrice, volume: QuoteVolume): F[Unit]
  def deleteQuote(id: QuoteID): F[Unit]
  def deleteAllQuotes(): F[Unit]
}

object QuoteService {

  def apply[F[_]: Sync](str: Storage[F, QuoteID, Quote], query: StorageQuery[F, QuoteID, Quote]): F[QuoteService[F]]   =
    Sync[F].pure(Impl(str, query))

  private case class Impl[F[_]: Sync](storage: Storage[F, QuoteID, Quote],
                                      query: StorageQuery[F, QuoteID, Quote]
                                     ) extends QuoteService[F] {
    override def newQuote(id: QuoteID, price: QuotePrice, volume: QuoteVolume): F[Unit] = for {
      now <- Sync[F].delay(Instant.now)
      _ <- storage.add(id, Quote(QuoteCreateAt(now), price, volume))
    } yield ()

    override def updateQuote(id: QuoteID, price: QuotePrice, volume: QuoteVolume): F[Unit] = {
      for {
        old <- OptionT(query.getById(id))  // TODO implement MonadError
        _ <- OptionT.liftF(storage.update(id, old.copy(quotePrice = price, quoteVolume = volume)))
      } yield ()
    }.getOrElse(())

    override def deleteQuote(id: QuoteID): F[Unit] = storage.delete(id)

    override def deleteAllQuotes(): F[Unit] = storage.deleteAll()
  }
}
