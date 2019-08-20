package com.maxkorolev.codility

object MissingInteger {

  def solution(a: Array[Int]): Int = {
    // write your code in Scala 2.12

    def checkSet(s: Set[Int], min: Int): (Set[Int], Int) =
      if (s contains min) checkSet(s - min, min + 1)
      else s -> min

    def go(arr: List[Int], s: Set[Int], min: Int): Int = arr match {
      case Nil => min
      case head :: tl if head == min =>
        val (nS, nMin) = checkSet(s, min + 1)
        go(tl, nS, nMin)
      case head :: tl if head > min => go(tl, s + head, min)
      case head :: tl               => go(tl, s, min)
    }

    go(a.toList, Set(), 1)
  }
}
