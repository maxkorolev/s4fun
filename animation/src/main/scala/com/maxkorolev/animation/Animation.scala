package com.maxkorolev.animation

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object Animation {

  sealed trait Direction
  case object Left extends Direction
  case object Right extends Direction
  case class Part(index: Int, direction: Direction)

  private def normalAnimate(init: String, speed: Int): Array[String] = {

    val len = init.length
    val parts = init.zipWithIndex.foldLeft(Nil: List[Part]) {
      case (acc, ('.', _)) => acc
      case (acc, ('L', idx)) => acc :+ Part(idx, Left)
      case (acc, ('R', idx)) => acc :+ Part(idx, Right)
    }

    val rightDist = parts.filter(_.direction == Right).map(_.index).sorted.headOption
      .map(idx => (len - 1 - idx) / speed + 1 ) getOrElse 0

    val leftDist =  parts.filter(_.direction == Left).map(_.index).sorted.lastOption
      .map(idx => idx / speed + 1) getOrElse 0

    (0 until (rightDist max leftDist)).foldLeft(List(parts)) {
      case (acc, _) =>

        val last = acc.last

        acc :+ last.map{
          case p @ Part(index, Left) => p.copy(index = index - speed)
          case p @ Part(index, Right) => p.copy(index = index + speed)
        }
    }.map { ps =>
      ps.foldLeft("." * len) {
        case (acc, p) if p.index < len && p.index >= 0 =>
          acc.substring(0, p.index) + "X" + acc.substring(p.index + 1)
        case (acc, _) => acc
      }
    }.toArray

  }

  def animate(init: String, speed: Int): Array[String] = macro animateImpl

  def animateImpl(c: blackbox.Context)(init: c.Expr[String], speed: c.Expr[Int]) = {
    import c.universe._
    init -> speed match {
      case (Expr(Literal(Constant(initValue: String))), Expr(Literal(Constant(speedValue: Int)))) =>
        val result = normalAnimate(initValue, speedValue).map(str => q"$str")

        q"..$result"
      case _ =>
        q"Array()"
    }
  }
}


