package com.maxkorolev.codility
import scala.annotation.tailrec

object Brackets {
  def solution(s: String): Int = {
    // write your code in Scala 2.12

    @tailrec
    def go(str: List[Char], stack: List[Char]): Boolean =
      (str, stack) match {
        case (Nil, st)         => st.isEmpty
        case ('(' :: tail, st) => go(tail, '(' :: st)
        case ('[' :: tail, st) => go(tail, '[' :: st)
        case ('{' :: tail, st) => go(tail, '{' :: st)

        case (')' :: tail, Nil) => false
        case (']' :: tail, Nil) => false
        case ('}' :: tail, Nil) => false

        case (')' :: tail, top :: t) if top == '(' => go(tail, t)
        case (']' :: tail, top :: t) if top == '[' => go(tail, t)
        case ('}' :: tail, top :: t) if top == '{' => go(tail, t)

        case (_, _) => false
      }

    if (go(s.toList, Nil)) 1 else 0
  }
}
