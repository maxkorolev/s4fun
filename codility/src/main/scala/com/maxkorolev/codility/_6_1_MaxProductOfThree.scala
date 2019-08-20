package com.maxkorolev.codility

object MaxProductOfThree {

  def solution(a: Array[Int]): Int = {
    // write your code in Scala 2.12

    val n = a.length
    val sorted = a.sorted

    sorted(0) * sorted(1) * sorted(2) max
      sorted(0) * sorted(1) * sorted(n - 1) max
      sorted(0) * sorted(n - 2) * sorted(n - 1) max
      sorted(n - 3) * sorted(n - 2) * sorted(n - 1)

  }
}
