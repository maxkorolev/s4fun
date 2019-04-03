package com.maxkorolev.mini_book

import scala.util.matching.Regex

trait Parser[F[_]] {
  def parse(s: String): F[Boolean]
}

object Parser {
  val NewOffer: Regex = "\\w+/O/N/\\d+/\\d+".r
  val UpdateOffer: Regex = "\\w+/O/U/\\d+/\\d+".r
  val DeleteOffer: Regex = "\\w+/O/D/\\d+/\\d+".r
  val DeleteAllOffer: Regex = "0/O/D/0/0".r
  val NewBid: Regex = "\\w+/B/N/\\d+/\\d+".r
  val UpdateBid: Regex = "\\w+/B/U/\\d+/\\d+".r
  val DeleteBid: Regex = "\\w+/B/D/\\d+/\\d+".r
  val DeleteAllBid: Regex = "0/B/D/0/0".r

}
