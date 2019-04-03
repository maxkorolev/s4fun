package com.maxkorolev.mini_book

import scala.util.matching.Regex

trait Parser[F[_]] {
  def parse(s: String): F[Unit]
}

object Parser {
  val NewOffer = "\\w+/O/N/[\\d]*[\\.]?[\\d]*/[\\d]*[\\.]?[\\d]*".r
  val UpdateOffer = "\\w+/O/U/[\\d]*[\\.]?[\\d]*/[\\d]*[\\.]?[\\d]*".r
  val DeleteOffer = "\\w+/O/D/.*".r
  val NewBid = "\\w+/B/N/\\d+/\\d+".r
  val UpdateBid = "\\w+/B/U/[\\d]*[\\.]?[\\d]*/[\\d]*[\\.]?[\\d]*".r
  val DeleteBid = "\\w+/B/D/.*".r

  def apply[F[_]](offerService: QuoteService[F], bidService: QuoteService[F]): Parser[F] = str => {
    str.split("/").toList match {
      case id :: "O" :: "N" ::price :: volume :: Nil =>
        offerService.newQuote(QuoteID(id), QuotePrice(BigDecimal(price)), QuoteVolume(BigDecimal(volume)))
      case id :: "O" :: "U" ::price :: volume :: Nil =>
        offerService.updateQuote(QuoteID(id), QuotePrice(BigDecimal(price)), QuoteVolume(BigDecimal(volume)))
      case id :: "O" :: "D" :: _ =>
        offerService.deleteQuote(QuoteID(id))
      case "0" :: "O" :: _ =>
        offerService.deleteAllQuotes()
      case id :: "B" :: "N" ::price :: volume :: Nil =>
        bidService.newQuote(QuoteID(id), QuotePrice(BigDecimal(price)), QuoteVolume(BigDecimal(volume)))
      case id :: "B" :: "U" ::price :: volume :: Nil =>
        bidService.updateQuote(QuoteID(id), QuotePrice(BigDecimal(price)), QuoteVolume(BigDecimal(volume)))
      case id :: "B" :: "D" :: _ =>
        bidService.deleteQuote(QuoteID(id))
      case "0" :: "B" :: _ =>
        bidService.deleteAllQuotes()
    }
  }
}
