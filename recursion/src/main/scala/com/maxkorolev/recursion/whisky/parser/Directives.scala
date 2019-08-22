package com.maxkorolev.recursion.whisky.parser

import java.util.UUID

import org.parboiled2._
import org.parboiled2.CharPredicate.{Digit19, HexDigit}

import scala.util.{Failure, Success}
import com.maxkorolev.recursion.whisky.ast

trait Directives { this: Parser with Tokens with Operations with Ignored =>

  def Directives = rule { Directive.+ ~> (_.toList) }

  def DirectivesConst = rule { DirectiveConst.+ ~> (_.toList) }

  def Directive = rule {
    Comments ~ trackPos ~ '@' ~ NameStrict ~ Arguments.? ~> {
      (comment, location, name, args) =>
        ast.Ast(location, ast.Directive(name, args getOrElse Nil, comment))
    }
  }

  def DirectiveConst = rule {
    Comments ~ trackPos ~ '@' ~ NameStrict ~ ArgumentsConst.? ~> {
      (comment, location, name, args) =>
        ast.Ast(location, ast.Directive(name, args getOrElse Nil, comment))
    }
  }

}
