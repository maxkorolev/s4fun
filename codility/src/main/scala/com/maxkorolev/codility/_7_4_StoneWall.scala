package com.maxkorolev.codility

object StoneWall {

  def solution(h: Array[Int]): Int = {
    // write your code in Scala 2.12

    def cleanStack(stack: List[Int], v: Int): (List[Int], Int) = stack match {
      case Nil if v == 0       => (Nil, 0)
      case Nil                 => (v :: Nil, 1)
      case h :: tail if h < v  => (h :: tail, 1)
      case h :: tail if h == v => (h :: tail, 0)
      case h :: tail if h > v  => cleanStack(tail, v)
    }

    def go(lst: List[Int], stack: List[Int], acc: Int): Int =
      (lst, stack) match {
        case (Nil, _)                     => acc
        case (0 :: t, Nil)                => go(t, Nil, acc)
        case (c :: t, Nil)                => go(t, c :: Nil, acc + 1)
        case (c :: t, st) if c == st.head => go(t, st, acc)
        case (c :: t, st) if c > st.head  => go(t, c :: st, acc + 1)
        case (c :: t, st) if c < st.head =>
          val (newStack, added) = cleanStack(st, c)
          go(t, newStack, acc + added)
      }

    go(h.toList, Nil, 0)
  }

  def budgetShopping(
      n: Int,
      bundleQuantities: Array[Int],
      bundleCosts: Array[Int]
  ): Int = {
    // Write your code here

    def go(lst: List[(Int, Int)], s: Int, res: Int): Int = lst match {
      case Nil           => res
      case lst if s <= 0 => res
      case (c, q) :: tl =>
        go(tl, s % c, res + (s / c) * q)
    }

    val a = (bundleCosts zip bundleQuantities).toArray

    (0 to a.length - 1)
      .map(idx => go(a.slice(idx, a.length).toList, n, 0))
      .toList
      .max

  }

}
