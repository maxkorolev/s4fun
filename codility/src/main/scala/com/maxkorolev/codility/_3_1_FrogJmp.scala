package com.maxkorolev.codility
import scala.math.BigDecimal.RoundingMode

object FrogJmp {
  def solution(x: Int, y: Int, d: Int): Int = {
    // write your code in Scala 2.12

    (BigDecimal(y - x) / d).setScale(0, RoundingMode.UP).toInt
  }
}
