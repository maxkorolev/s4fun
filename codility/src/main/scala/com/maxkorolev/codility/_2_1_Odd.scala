package com.maxkorolev.codility

import scala.collection.JavaConverters._
import scala.annotation.tailrec

// you can write to stdout for debugging purposes, e.g.
// println("this is a debug message")

object Odd {
  def solution(a: Array[Int]): Int = {
    // write your code in Scala 2.12

    @tailrec
    def collect(in: List[Int], acc: Int): Int =
      if (in.isEmpty) acc
      else collect(in.tail, in.head ^ acc)

    collect(a.toList, 0)
  }
}
