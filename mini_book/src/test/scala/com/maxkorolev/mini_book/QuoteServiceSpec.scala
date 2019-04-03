package com.maxkorolev.mini_book

import cats.effect._
import org.scalatest._

import scala.concurrent.ExecutionContext

class QuoteServiceSpec extends FlatSpec with Matchers {

  implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  "Storage" should "be able add, update and delete elements" in {

    (for {
      storage <- Storage[IO, QuoteID, Quote]
      query <- StorageQuery(storage)
      quotes <- QuoteService(storage, query)

      _ <- quotes.newQuote(QuoteID("Q1"), QuotePrice(1000), QuoteVolume(10))
      _ <- quotes.updateQuote(QuoteID("Q1"), QuotePrice(200), QuoteVolume(5))

      q <- query.getById(QuoteID("Q1"))

    } yield {
      q.get.quotePrice shouldEqual QuotePrice(200)
    }).unsafeRunSync()
  }
}
