package com.maxkorolev.codility

object CyclicRotation {
  def solution(a: Array[Int], k: Int): Array[Int] = {
    // write your code in Scala 2.12
    if (a.length == 0) a
    else a.takeRight(k % a.length) ++ a.dropRight(k % a.length)
  }
}
