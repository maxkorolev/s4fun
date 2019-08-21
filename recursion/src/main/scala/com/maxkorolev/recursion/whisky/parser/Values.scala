package com.maxkorolev.recursion.whisky.parser

import java.util.UUID

import org.parboiled2._
import org.parboiled2.CharPredicate.{Digit19, HexDigit}

import scala.util.{Failure, Success}

trait Values { this: Parser with Tokens with Ignored with Operations =>

  def ValueConst: Rule1[ast.Value] = rule {
    NumberValue | StringValue | BooleanValue | NullValue | EnumValue | ListValueConst | ObjectValueConst
  }

  def Value: Rule1[ast.Value] = rule {
    Comments ~ trackPos ~ Variable ~> { (comment, location, name) =>
      ast.VariableValue(name, comment, location)
    } |
      NumberValue |
      StringValue |
      BooleanValue |
      NullValue |
      EnumValue |
      ListValue |
      ObjectValue
  }

  def BooleanValue = rule {
    Comments ~ trackPos ~ True ~> { (comment, location) =>
      ast.BooleanValue(true, comment, location)
    } |
      Comments ~ trackPos ~ False ~> { (comment, location) =>
        ast.BooleanValue(false, comment, location)
      }
  }

  def True = rule { Keyword("true") }

  def False = rule { Keyword("false") }

  def Null = rule { Keyword("null") }

  def NullValue = rule {
    Comments ~ trackPos ~ Null ~> { (comment, location) =>
      ast.NullValue(comment, location)
    }
  }

  def EnumValue = rule {
    Comments ~ !(True | False) ~ trackPos ~ Name ~> {
      (comment, location, name) =>
        ast.EnumValue(name, comment, location)
    }
  }

  def ListValueConst = rule {
    Comments ~ trackPos ~ wsNoComment('[') ~ ValueConst.* ~ wsNoComment(']') ~> {
      (comment, location, v) =>
        ast.ListValue(v.toVector, comment, location)
    }
  }

  def ListValue = rule {
    Comments ~ trackPos ~ wsNoComment('[') ~ Value.* ~ wsNoComment(']') ~> {
      (comment, location, v) =>
        ast.ListValue(v.toVector, comment, location)
    }
  }

  def ObjectValueConst = rule {
    Comments ~ trackPos ~ wsNoComment('{') ~ ObjectFieldConst.* ~ wsNoComment(
      '}'
    ) ~> { (comment, location, f) =>
      ast.ObjectValue(f.toVector, comment, location)
    }
  }

  def ObjectValue = rule {
    Comments ~ trackPos ~ wsNoComment('{') ~ ObjectField.* ~ wsNoComment('}') ~> {
      (comment, location, f) =>
        ast.ObjectValue(f.toVector, comment, location)
    }
  }

  def ObjectFieldConst = rule {
    Comments ~ trackPos ~ Name ~ wsNoComment(':') ~ ValueConst ~> {
      (comment, location, name, value) =>
        ast.ObjectField(name, value, comment, location)
    }
  }

  def ObjectField = rule {
    Comments ~ trackPos ~ Name ~ wsNoComment(':') ~ Value ~> {
      (comment, location, name, value) =>
        ast.ObjectField(name, value, comment, location)
    }
  }

}
