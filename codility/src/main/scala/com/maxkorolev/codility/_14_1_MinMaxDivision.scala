package com.maxkorolev.codility
import scala.annotation.tailrec

object MinMaxDivision {
  def solution(k: Int, m: Int, a: Array[Int]): Int = {
    // write your code in Scala 2.12

    @tailrec
    def binarySearch(lst: List[Int], min: Int, max: Int): Int = {

      val mid = (min + max) / 2

      val blocks = countBlocks(lst, mid, 0, 0)

      if (blocks == k) max
      else if (blocks < k) binarySearch(lst, min, mid - 1)
      else binarySearch(lst, mid + 1, max)
    }

    def countBlocks(lst: List[Int], mid: Int, sum: Int, acc: Int): Int =
      lst match {
        case Nil => acc
        case head :: tl if sum + head > mid =>
          countBlocks(tl, mid, head, acc + 1)
        case head :: tl =>
          countBlocks(tl, mid, sum + head, acc)
      }

    val maxSum = a.sum
    val minSum = a.max

    binarySearch(a.toList, minSum, maxSum)

  }
}
