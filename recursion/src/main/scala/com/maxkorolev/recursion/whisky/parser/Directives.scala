package com.maxkorolev.recursion.whisky.parser

import java.util.UUID

import org.parboiled2._
import org.parboiled2.CharPredicate.{Digit19, HexDigit}

import scala.util.{Failure, Success}

trait Directives { this: Parser with Tokens with Operations with Ignored =>

  def Directives = rule { Directive.+ ~> (_.toVector) }

  def DirectivesConst = rule { DirectiveConst.+ ~> (_.toVector) }

  def Directive = rule {
    Comments ~ trackPos ~ '@' ~ NameStrict ~ (Arguments.? ~> (_ getOrElse Vector.empty)) ~> {
      (comment, location, name, args) =>
        ast.Directive(name, args, comment, location)
    }
  }

  def DirectiveConst = rule {
    Comments ~ trackPos ~ '@' ~ NameStrict ~ (ArgumentsConst.? ~> (_ getOrElse Vector.empty)) ~> {
      (comment, location, name, args) =>
        ast.Directive(name, args, comment, location)
    }
  }

}