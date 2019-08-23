package com.maxkorolev.recursion.whisky.parser

import java.util.UUID

import org.parboiled2._
import org.parboiled2.CharPredicate.{Digit19, HexDigit}

import scala.util.{Failure, Success}
import com.maxkorolev.recursion.whisky.ast
import cats.syntax.all._
import cats.instances.either._

class QueryParser private (
    val input: ParserInput,
    val sourceId: String,
    val experimentalFragmentVariables: Boolean = false,
    val parseLocations: Boolean = true,
    val parseComments: Boolean = true
) extends Parser
    with Tokens
    with Ignored
    with Document
    with Operations
    with Fragments
    with Values
    with Directives
    with Types
    with TypeSystemDefinitions

object QueryParser {
  def parse(
      input: String,
      config: ParserConfig = ParserConfig.default
  ): Either[Throwable, ast.Ast] = {
    parse(ParserInput(input), config)
  }

  def parse(
      input: ParserInput,
      config: ParserConfig
  ): Either[Throwable, ast.Ast] = {
    val id = config.sourceIdFn(input)
    val parser = new QueryParser(
      input,
      id,
      config.experimentalFragmentVariables,
      config.parseLocations,
      config.parseComments
    )

    parser.Document.run() match {
      case Success(res) =>
        Right(res)
      case Failure(e: ParseError) =>
        Left(SyntaxError(parser, input, e))
      case Failure(e) => Left(e)
    }
  }

  // def parseInput(
  //     input: String
  // ): Either[Throwable, ast.Ast] =
  //   parseInput(ParserInput(input))

  // def parseInput(
  //     input: ParserInput
  // ): Either[Throwable, ast.Ast] = {
  //   val parser = new QueryParser(input, "")

  //   parser.InputDocument.run() match {
  //     case Success(res) if res.values.nonEmpty =>
  //       Right(res.values.head)
  //     case Success(res) =>
  //       Left(
  //         new IllegalArgumentException(
  //           "Input document does not contain any value definitions."
  //         )
  //       )
  //     case Failure(e: ParseError) =>
  //       Left(SyntaxError(parser, input, e))
  //     case Failure(e) => Left(e)
  //   }
  // }

  def parseInputDocument(
      input: String,
      config: ParserConfig = ParserConfig.default
  ): Either[Throwable, ast.Ast] =
    parseInputDocument(ParserInput(input), config)

  def parseInputDocument(
      input: ParserInput,
      config: ParserConfig
  ): Either[Throwable, ast.Ast] = {
    val id = config.sourceIdFn(input)
    val parser = new QueryParser(
      input,
      id,
      config.experimentalFragmentVariables,
      config.parseLocations,
      config.parseComments
    )

    parser.InputDocument.run() match {
      case Success(res) =>
        Right(res)
      case Failure(e: ParseError) =>
        Left(SyntaxError(parser, input, e))
      case Failure(e) => Left(e)
    }
  }

  // def parseInputWithVariables(
  //     input: String
  // ): Either[Throwable, ast.Ast] =
  //   parseInputWithVariables(ParserInput(input))

  // def parseInputWithVariables(
  //     input: ParserInput
  // ): Either[Throwable, ast.Ast] = {
  //   val parser = new QueryParser(input, "")

  //   parser.InputDocumentWithVariables.run() match {
  //     case Success(res) if res.values.nonEmpty =>
  //       Right(res.values.head)
  //     case Success(res) =>
  //       Left(
  //         new IllegalArgumentException(
  //           "Input document does not contain any value definitions."
  //         )
  //       )
  //     case Failure(e: ParseError) =>
  //       Left(SyntaxError(parser, input, e))
  //     case Failure(e) => Left(e)
  //   }
  // }

  def parseInputDocumentWithVariables(
      input: String,
      config: ParserConfig = ParserConfig.default
  ): Either[Throwable, ast.Ast] =
    parseInputDocumentWithVariables(ParserInput(input), config)

  def parseInputDocumentWithVariables(
      input: ParserInput,
      config: ParserConfig
  ): Either[Throwable, ast.Ast] = {
    val id = config.sourceIdFn(input)
    val parser = new QueryParser(input, id)

    parser.InputDocumentWithVariables.run() match {
      case Success(res) =>
        Right(res)
      case Failure(e: ParseError) =>
        Left(SyntaxError(parser, input, e))
      case Failure(e) => Left(e)
    }
  }
}
