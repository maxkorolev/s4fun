package com.maxkorolev.codility

object PassingCars {

  def solution(a: Array[Int]): Int = {
    // write your code in Scala 2.12

    def go(lst: List[Int], zeros: Int, acc: Int): Option[Int] =
      lst match {
        case _ if zeros > 1000000000 => None
        case Nil                     => Some(acc)
        case 0 :: tail               => go(tail, zeros + 1, acc)
        case 1 :: tail               => go(tail, zeros, acc + zeros)
      }

    go(a.toList, 0, 0) getOrElse -1
  }
}
