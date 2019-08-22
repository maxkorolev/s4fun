package com.maxkorolev.recursion.whisky.parser

import java.util.UUID

import org.parboiled2._
import org.parboiled2.CharPredicate.{Digit19, HexDigit}

import scala.util.{Failure, Success}
import com.maxkorolev.recursion.whisky.ast

trait Types { this: Parser with Tokens with Ignored =>
  def Type: Rule1[ast.Ast] = rule { NonNullType | ListType | NamedType }

  def TypeName = rule { Name }

  def NamedType = rule {
    Ignored.* ~ trackPos ~ TypeName ~> { (location, name) =>
      ast.Ast(location, ast.NamedType(name))
    }
  }

  def ListType = rule {
    trackPos ~ ws('[') ~ Type ~ wsNoComment(']') ~> { (location, tpe) =>
      ast.Ast(location, ast.ListType(tpe))
    }
  }

  def NonNullType = rule {
    trackPos ~ TypeName ~ wsNoComment('!') ~> { (location, name) =>
      ast.Ast(location, ast.NotNullType(ast.Ast(location, ast.NamedType(name))))
    } | trackPos ~ ListType ~ wsNoComment('!') ~> { (location, tpe) =>
      ast.Ast(location, ast.NotNullType(tpe))
    }
  }
}
