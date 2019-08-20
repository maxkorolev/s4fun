package com.maxkorolev.codility

object TapeEquilibrium {
  def solution(a: Array[Int]): Int = {
    // write your code in Scala 2.12

    val pref = a
      .foldLeft(Nil: List[Int]) {
        case (Nil, v)    => v :: Nil
        case (h :: t, v) => (h + v) :: h :: t
      }
      .tail

    val prefRev = a
      .foldRight(Nil: List[Int]) {
        case (v, Nil)    => v :: Nil
        case (v, h :: t) => (h + v) :: h :: t
      }
      .tail
      .reverse

    (pref zip prefRev).map(v => v._1 - v._2).map(Math.abs).min
  }
}
