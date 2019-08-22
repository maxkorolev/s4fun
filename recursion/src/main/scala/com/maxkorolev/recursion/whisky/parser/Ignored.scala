package com.maxkorolev.recursion.whisky.parser

import java.util.UUID

import org.parboiled2._
import org.parboiled2.CharPredicate.{Digit19, HexDigit}

import scala.util.{Failure, Success}
import com.maxkorolev.recursion.whisky.ast
import higherkindness.droste.data._
import sangria.ast.AstNode

trait Ignored extends PositionTracking { this: Parser =>
  def parseComments: Boolean

  val WhiteSpace = CharPredicate("\u0009\u0020")

  def CRLF = rule { '\u000D' ~ '\u000A' }

  val LineTerminator = CharPredicate("\u000A")

  val UnicodeBOM = CharPredicate('\uFEFF')

  def Ignored = rule {
    quiet(
      UnicodeBOM | WhiteSpace | (CRLF | LineTerminator) ~ trackNewLine | Comment | ','
    )
  }

  def IgnoredNoComment = rule {
    quiet(
      UnicodeBOM | WhiteSpace | (CRLF | LineTerminator) ~ trackNewLine | ','
    )
  }

  def Comments = rule {
    test(parseComments) ~ CommentCap.* ~ Ignored.* ~> (_.toList) | CommentNoCap.* ~ Ignored.* ~ push(
      Nil
    )
  }

  def CommentCap = rule {
    trackPos ~ "#" ~ capture(CommentChar.*) ~ IgnoredNoComment.* ~> {
      (location, comment) =>
        ast.Ast(location, ast.Comment(comment))
    }
  }

  def CommentNoCap: Rule0 = rule { "#" ~ CommentChar.* ~ IgnoredNoComment.* }

  def Comment = rule { "#" ~ CommentChar.* }

  def CommentChar = rule { !(CRLF | LineTerminator) ~ ANY }

  def ws(char: Char): Rule0 = rule { quiet(Ignored.* ~ ch(char) ~ Ignored.*) }

  def wsNoComment(char: Char): Rule0 = rule {
    quiet(Ignored.* ~ ch(char) ~ IgnoredNoComment.*)
  }

  def ws(s: String): Rule0 = rule { quiet(Ignored.* ~ str(s) ~ Ignored.*) }

  def wsCapture(s: String) = rule {
    quiet(Ignored.* ~ capture(str(s)) ~ IgnoredNoComment.*)
  }

}
