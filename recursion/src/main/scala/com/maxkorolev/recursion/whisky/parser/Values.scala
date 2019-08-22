package com.maxkorolev.recursion.whisky.parser

import java.util.UUID

import org.parboiled2._
import org.parboiled2.CharPredicate.{Digit19, HexDigit}

import scala.util.{Failure, Success}
import com.maxkorolev.recursion.whisky.ast

trait Values { this: Parser with Tokens with Ignored with Operations =>

  def ValueConst: Rule1[ast.Ast] = rule {
    NumberValue | StringValue | BooleanValue | NullValue | EnumValue | ListValueConst | ObjectValueConst
  }

  def Value: Rule1[ast.Ast] = rule {
    Comments ~ trackPos ~ Variable ~> { (comment, location, name) =>
      ast.Ast(location, ast.VariableValue(name, comment))
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
      ast.Ast(location, ast.BooleanValue(true, comment))
    } |
      Comments ~ trackPos ~ False ~> { (comment, location) =>
        ast.Ast(location, ast.BooleanValue(false, comment))
      }
  }

  def True = rule { Keyword("true") }

  def False = rule { Keyword("false") }

  def Null = rule { Keyword("null") }

  def NullValue = rule {
    Comments ~ trackPos ~ Null ~> { (comment, location) =>
      ast.Ast(location, ast.NullValue(comment))
    }
  }

  def EnumValue = rule {
    Comments ~ !(True | False) ~ trackPos ~ Name ~> {
      (comment, location, name) =>
        ast.Ast(location, ast.EnumValue(name, comment))
    }
  }

  def ListValueConst = rule {
    Comments ~ trackPos ~ wsNoComment('[') ~ ValueConst.* ~ wsNoComment(']') ~> {
      (comment, location, v) =>
        ast.Ast(location, ast.ListValue(v.toList, comment))
    }
  }

  def ListValue = rule {
    Comments ~ trackPos ~ wsNoComment('[') ~ Value.* ~ wsNoComment(']') ~> {
      (comment, location, v) =>
        ast.Ast(location, ast.ListValue(v.toList, comment))
    }
  }

  def ObjectValueConst = rule {
    Comments ~ trackPos ~ wsNoComment('{') ~ ObjectFieldConst.* ~ wsNoComment(
      '}'
    ) ~> { (comment, location, f) =>
      ast.Ast(location, ast.ObjectValue(f.toList, comment))
    }
  }

  def ObjectValue = rule {
    Comments ~ trackPos ~ wsNoComment('{') ~ ObjectField.* ~ wsNoComment('}') ~> {
      (comment, location, f) =>
        ast.Ast(location, ast.ObjectValue(f.toList, comment))
    }
  }

  def ObjectFieldConst = rule {
    Comments ~ trackPos ~ Name ~ wsNoComment(':') ~ ValueConst ~> {
      (comment, location, name, value) =>
        ast.Ast(location, ast.ObjectField(name, value, comment))
    }
  }

  def ObjectField = rule {
    Comments ~ trackPos ~ Name ~ wsNoComment(':') ~ Value ~> {
      (comment, location, name, value) =>
        ast.Ast(location, ast.ObjectField(name, value, comment))
    }
  }

}
