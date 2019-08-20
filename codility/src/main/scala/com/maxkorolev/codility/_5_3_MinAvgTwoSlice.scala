package com.maxkorolev.codility

object MinAvgTwoSlice {
  def solution(a: Array[Int]): Int = {
    // write your code in Scala 2.12

    def go(lst: List[Int], min: Int, minIdx: Int, idx: Int): Int = lst match {
      case _ :: Nil => minIdx
      case h1 :: h2 :: tl if h1 + h2 < min =>
        go(h2 :: tl, h1 + h2, idx, idx + 1)
      case h1 :: h2 :: tl =>
        go(h2 :: tl, min, minIdx, idx + 1)
    }
    go(a.toList, Int.MaxValue, 0, 0)

  }
}
