package com.maxkorolev.mini_book

import fastparse._, NoWhitespace._

trait Parser[F[_]] {
  def parse(s: String): F[Unit]
}

object Parser {

  def sep[_: P]: P[Unit] = P("/")
  def string[_: P]: P[String] = P( (!sep ~ AnyChar).rep.! )

  def digit[_: P]: P[Unit] = P( CharIn("0-9") )
  def decimal[_: P]: P[BigDecimal] = P( pointfloat | exponentfloat | intpart )
  def pointfloat[_: P]: P[BigDecimal] = P( intpart.? ~ fraction | intpart ~ "." ).!.map(BigDecimal(_))
  def exponentfloat[_: P]: P[BigDecimal] = P( (intpart | pointfloat) ~ exponent ).!.map(BigDecimal(_))
  def intpart[_: P]: P[BigDecimal] = P( digit.rep(1) ).!.map(BigDecimal(_))
  def fraction[_: P]: P[Unit] = P( "." ~ digit.rep(1) )
  def exponent[_: P]: P[Unit] = P( ("e" | "E") ~ ("+" | "-").? ~ digit.rep(1) )

  def quoteID[_: P]: P[QuoteID] = string.map(QuoteID.apply)
  def quotePrice[_: P]: P[QuotePrice] = decimal.map(QuotePrice.apply)
  def quoteVolume[_: P]: P[QuoteVolume] = decimal.map(QuoteVolume.apply)
  def quoteType[_: P]: P[QuoteType] = P("O").map(_ => Offer) | P("B").map(_ => Bid)
  def quoteOperation[_: P]: P[QuoteOperation] =
    P("N").map(_ => NewQuote) | P("D").map(_ => DeleteQuote) | P("U").map(_ => UpdateQuote)

  def apply[F[_]](offerService: QuoteService[F], bidService: QuoteService[F]): Parser[F] = str => {

    def parser[_: P]: P[F[Unit]] = {
      P(quoteID ~ "/" ~ quoteType ~ "/" ~ quoteOperation ~ "/" ~ quotePrice ~ "/" ~ quoteVolume ~ End).map {

        case (id, Offer, NewQuote, price, volume) =>
          offerService.newQuote(id, price, volume)
        case (id, Offer, UpdateQuote, price, volume) =>
          offerService.updateQuote(id, price, volume)
        case (QuoteID("0"), Offer, DeleteQuote, _, _) =>
          offerService.deleteAllQuotes()
        case (id, Offer, DeleteQuote, _, _) =>
          offerService.deleteQuote(id)

        case (id, Bid, NewQuote, price, volume) =>
          bidService.newQuote(id, price, volume)
        case (id, Bid, UpdateQuote, price, volume) =>
          bidService.updateQuote(id, price, volume)
        case (QuoteID("0"), Bid, DeleteQuote, _, _) =>
          bidService.deleteAllQuotes()
        case (id, Bid, DeleteQuote, _, _) =>
          bidService.deleteQuote(id)

      }
    }


    val Parsed.Success(res, _) = parse(str, parser(_)) // TODO implement MonadError
    res
  }
}
