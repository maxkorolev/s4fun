package com.maxkorolev.codility

object PermCheck {
  def solution(a: Array[Int]): Int = {
    // write your code in Scala 2.12

    if ((a ++ (1 to a.length)).reduce(_ ^ _) == 0) 1 else 0
  }
}
