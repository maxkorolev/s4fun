package com.maxkorolev.codility
import scala.math.BigDecimal.RoundingMode

object FrogRiverOne {

  def solution(x: Int, a: Array[Int]): Int = {
    // write your code in Scala 2.12
    val xBig = BigDecimal(x)
    val stamp = ((xBig + 1) * xBig / 2).setScale(0)

    def go(arr: List[Int], s: Set[Int], sum: BigDecimal, idx: Int): Int =
      arr match {
        case Nil                               => -1
        case head :: tl if s contains head     => go(tl, s, sum, idx + 1)
        case head :: tl if sum + head == stamp => idx
        case head :: tl                        => go(tl, s + head, sum + head, idx + 1)
      }

    go(a.toList, Set(), 0, 0)
  }
}
