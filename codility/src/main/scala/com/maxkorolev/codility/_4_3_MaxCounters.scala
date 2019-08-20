package com.maxkorolev.codility

object MaxCounters {

  def solution(n: Int, a: Array[Int]): Array[Int] = {
    // write your code in Scala 2.12

    def defaultMap(value: Int): Map[Int, Int] = Map().withDefault(_ => value)

    val (resMap, _) = a.foldLeft((defaultMap(0), 0)) {
      case ((m, currMax), idx) if idx <= n && m(idx) + 1 > currMax =>
        m.updated(idx, m(idx) + 1) -> (m(idx) + 1)
      case ((m, currMax), idx) if idx <= n =>
        m.updated(idx, m(idx) + 1) -> currMax
      case ((m, currMax), idx) =>
        defaultMap(currMax) -> currMax
    }

    (1 to n).toList.map(idx => resMap(idx)).toArray
  }

}
