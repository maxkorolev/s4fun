package com.maxkorolev.mini_book

import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.traverse._
import cats.instances.list._
import cats.syntax.show._

object MiniBook extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {

    val inputStr =
      """Q1/O/N/1.31/1000000
        |Q2/B/N/1.21/1000000
        |Q3/B/N/1.22/1000000
        |Q4/B/N/1.20/1000000
        |Q5/B/N/1.20/1000000
        |Q6/O/N/1.32/1000000
        |Q7/O/N/1.33/200000
        |Q5/B/U/1.20/500000
        |Q7/O/U/1.33/100000
        |Q7/O/D/0/0""".stripMargin

    for {
      _ <- IO.delay(println("hello"))

      offerStorage <- Storage[IO, QuoteID, Quote[Offer.type]]
      offerQuery <- StorageQuery(offerStorage)
      offers <- QuoteService(offerStorage, offerQuery)

      bidStorage <- Storage[IO, QuoteID, Quote[Bid.type]]
      bidQuery <- StorageQuery(bidStorage)
      bids <- QuoteService(bidStorage, bidQuery)

      parser = Parser(offers, bids)

      _ <- IO.delay(println(s"Lets move some book\r\n$inputStr\r\n\r\n"))

      _ <- inputStr.split("\n").toList.map(parser.parse).sequence

      offs <- offerQuery.getSorted
      bds <- bidQuery.getSorted

      _ <- IO.delay(println("OFFER"))
      _ <- IO.delay(println(offs.map(_.show).mkString("\r\n")))
      _ <- IO.delay(println(""))
      _ <- IO.delay(println("BID"))
      _ <- IO.delay(println(bds.map(_.show).mkString("\r\n")))

    } yield ExitCode.Success

  }
}
