package com.maxkorolev

import java.time.Instant

import cats.Show

package object mini_book {

  case class QuoteID(value: String) extends AnyVal

  sealed trait QuoteType
  case object Bid extends QuoteType
  case object Offer extends QuoteType

  sealed trait QuoteOperation
  case object NewQuote extends QuoteOperation
  case object UpdateQuote extends QuoteOperation
  case object DeleteQuote extends QuoteOperation

  case class QuoteCreateAt(value: Instant) extends AnyVal
  case class QuotePrice(value: BigDecimal) extends AnyVal
  case class QuoteVolume(value: BigDecimal) extends AnyVal

  case class Quote(quoteCreateAt: QuoteCreateAt,
                   quotePrice: QuotePrice,
                   quoteVolume: QuoteVolume,
                  )

  sealed trait QuoteError extends RuntimeException
  final case class QuoteNotFound(id: QuoteID) extends QuoteError

  implicit val orderingQuoteID: Ordering[QuoteID] = Ordering.by(v => v.value)
  implicit val orderingQuote: Ordering[Quote] = Ordering.by(v => -v.quotePrice.value)

  implicit val showQuote: Show[(QuoteID, Quote)] = Show.show {
    case (id, q)=> s"${id.value}/${q.quotePrice.value}/${q.quoteVolume.value}"
  }

}
