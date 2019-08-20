package com.maxkorolev.codility
import scala.math.BigDecimal.RoundingMode

object PermMissingElem {
  def solution(a: Array[Int]): Int = {
    // write your code in Scala 2.12

    // (BigDecimal((a.length + 2) * (a.length + 1)) / 2)
    //   .setScale(0, RoundingMode.DOWN)
    //   .toInt - a.sum

    (a ++ (1 to a.length + 1)).reduce(_ ^ _)
  }
}
