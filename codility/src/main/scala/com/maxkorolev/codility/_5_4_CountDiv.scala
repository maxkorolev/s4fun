package com.maxkorolev.codility

object CountDiv {

  def solution(a: Int, b: Int, k: Int): Int = {
    // write your code in Scala 2.12

    (b / k) - (a / k) + (if (a % k == 0) 1 else 0)
  }
}
