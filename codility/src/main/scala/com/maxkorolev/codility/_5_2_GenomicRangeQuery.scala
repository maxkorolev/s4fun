package com.maxkorolev.codility

object GenomicRangeQuery {

  def solution(s: String, p: Array[Int], q: Array[Int]): Array[Int] = {
    // write your code in Scala 2.12

    def check(ch: Char) = ch match {
      case 'A' => 1
      case 'C' => 2
      case 'G' => 3
      case 'T' => 4
    }
    val arr = s.map(check).toArray

    (0 to p.size - 1)
      .map(idx => arr.slice(p(idx), q(idx) + 1).min)
      .toArray
  }
}
