package com.maxkorolev.recursion.whisky.parser

import java.util.UUID

import org.parboiled2._
import org.parboiled2.CharPredicate.{Digit19, HexDigit}

import scala.util.{Failure, Success}

trait Tokens extends StringBuilding with PositionTracking {
  this: Parser with Ignored =>

  def Token = rule { Punctuator | Name | NumberValue | StringValue }

  val PunctuatorChar = CharPredicate("!$():=@[]{|}")

  def Punctuator = rule { PunctuatorChar | Ellipsis }

  def Ellipsis = rule { quiet(str("...") ~ IgnoredNoComment.*) }

  val NameFirstChar = CharPredicate.Alpha ++ '_'

  val NameChar = NameFirstChar ++ CharPredicate.Digit

  def NameStrict = rule {
    capture(NameFirstChar ~ NameChar.*) ~ IgnoredNoComment.*
  }

  def Name = rule { Ignored.* ~ NameStrict }

  def NumberValue = rule {
    atomic(
      Comments ~ trackPos ~ IntegerValuePart ~ FloatValuePart.? ~ IgnoredNoComment.*
    ) ~> { (comment, location, intPart, floatPart) =>
      floatPart map (
          f => ast.BigDecimalValue(BigDecimal(intPart + f), comment, location)
      ) getOrElse ast.BigIntValue(BigInt(intPart), comment, location)
    }
  }

  def FloatValuePart = rule {
    atomic(capture(FractionalPart ~ ExponentPart.? | ExponentPart))
  }

  def FractionalPart = rule { '.' ~ Digit.+ }

  def IntegerValuePart = rule { capture(NegativeSign.? ~ IntegerPart) }

  def IntegerPart = rule { ch('0') | NonZeroDigit ~ Digit.* }

  def ExponentPart = rule { ExponentIndicator ~ Sign.? ~ Digit.+ }

  def ExponentIndicator = rule { ch('e') | ch('E') }

  def Sign = rule { ch('-') | '+' }

  val NegativeSign = '-'

  val NonZeroDigit = Digit19

  def Digit = rule { ch('0') | NonZeroDigit }

  def StringValue = rule { BlockStringValue | NonBlockStringValue }

  def BlockStringValue = rule {
    Comments ~ trackPos ~ BlockString ~ clearSB() ~ BlockStringCharacters ~ BlockString ~ push(
      sb.toString
    ) ~ IgnoredNoComment.* ~> { (comment, location, s) =>
      ast.StringValue(
        StringUtil.blockStringValue(s),
        true,
        Some(s),
        comment,
        location
      )
    }
  }

  def BlockStringCharacters = rule {
    (BlockStringCharacter | BlockStringEscapedChar).*
  }

  def BlockString = rule { str("\"\"\"") }

  def QuotedBlockString = rule { str("\\\"\"\"") }

  def BlockStringCharacter = rule {
    !(QuotedBlockString | BlockString) ~ ((CRLF | LineTerminator) ~ trackNewLine | ANY) ~ appendSB()
  }

  def BlockStringEscapedChar = rule {
    QuotedBlockString ~ appendSB("\"\"\"")
  }

  def NormalCharacter = rule {
    !(QuoteBackslash | LineTerminator) ~ ANY ~ appendSB()
  }

  def NonBlockStringValue = rule {
    Comments ~ trackPos ~ '"' ~ clearSB() ~ Characters ~ '"' ~ push(sb.toString) ~ IgnoredNoComment.* ~> {
      (comment, location, s) =>
        ast.StringValue(s, false, None, comment, location)
    }
  }

  def Characters = rule { (NormalCharacter | '\\' ~ EscapedChar).* }

  val QuoteBackslash = CharPredicate("\"\\")

  def EscapedChar = rule {
    QuoteBackslash ~ appendSB() |
      'b' ~ appendSB('\b') |
      'f' ~ appendSB('\f') |
      'n' ~ appendSB('\n') |
      'r' ~ appendSB('\r') |
      't' ~ appendSB('\t') |
      Unicode ~> { code =>
        sb.append(code.asInstanceOf[Char]); ()
      }
  }

  def Unicode = rule {
    'u' ~ capture(4 times HexDigit) ~> (Integer.parseInt(_, 16))
  }

  def Keyword(s: String) = rule {
    atomic(Ignored.* ~ s ~ !NameChar ~ IgnoredNoComment.*)
  }
}
