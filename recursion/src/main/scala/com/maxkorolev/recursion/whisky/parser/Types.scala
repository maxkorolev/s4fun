package com.maxkorolev.recursion.whisky.parser

import java.util.UUID

import org.parboiled2._
import org.parboiled2.CharPredicate.{Digit19, HexDigit}

import scala.util.{Failure, Success}

trait Types { this: Parser with Tokens with Ignored =>
  def Type: Rule1[ast.Type] = rule { NonNullType | ListType | NamedType }

  def TypeName = rule { Name }

  def NamedType = rule {
    Ignored.* ~ trackPos ~ TypeName ~> { (location, name) =>
      ast.NamedType(name, location)
    }
  }

  def ListType = rule {
    trackPos ~ ws('[') ~ Type ~ wsNoComment(']') ~> { (location, tpe) =>
      ast.ListType(tpe, location)
    }
  }

  def NonNullType = rule {
    trackPos ~ TypeName ~ wsNoComment('!') ~> { (location, name) =>
      ast.NotNullType(ast.NamedType(name, location), location)
    } | trackPos ~ ListType ~ wsNoComment('!') ~> { (location, tpe) =>
      ast.NotNullType(tpe, location)
    }
  }
}
