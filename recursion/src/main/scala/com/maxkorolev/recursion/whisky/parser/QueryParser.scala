package com.maxkorolev.recursion.whisky.parser

import java.util.UUID

import org.parboiled2._
import org.parboiled2.CharPredicate.{Digit19, HexDigit}

import scala.util.{Failure, Success}

class QueryParser private (
    val input: ParserInput,
    val sourceId: String,
    val legacyImplementsInterface: Boolean = false,
    val legacyEmptyFields: Boolean = false,
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
  def parse(input: String, config: ParserConfig = ParserConfig.default)(
      implicit scheme: DeliveryScheme[ast.Document]
  ): scheme.Result = {
    parse(ParserInput(input), config)(scheme)
  }

  def parse(input: ParserInput, config: ParserConfig)(
      implicit scheme: DeliveryScheme[ast.Document]
  ): scheme.Result = {
    val id = config.sourceIdFn(input)
    val parser = new QueryParser(
      input,
      id,
      config.legacyImplementsInterface,
      config.legacyEmptyFields,
      config.experimentalFragmentVariables,
      config.parseLocations,
      config.parseComments
    )

    parser.Document.run() match {
      case Success(res) =>
        scheme.success(
          res.copy(sourceMapper = config.sourceMapperFn(id, input))
        )
      case Failure(e: ParseError) =>
        scheme.failure(SyntaxError(parser, input, e))
      case Failure(e) => scheme.failure(e)
    }
  }

  def parseInput(
      input: String
  )(implicit scheme: DeliveryScheme[ast.Value]): scheme.Result =
    parseInput(ParserInput(input))(scheme)

  def parseInput(
      input: ParserInput
  )(implicit scheme: DeliveryScheme[ast.Value]): scheme.Result = {
    val parser = new QueryParser(input, "")

    parser.InputDocument.run() match {
      case Success(res) if res.values.nonEmpty =>
        scheme.success(res.values.head)
      case Success(res) =>
        scheme.failure(
          new IllegalArgumentException(
            "Input document does not contain any value definitions."
          )
        )
      case Failure(e: ParseError) =>
        scheme.failure(SyntaxError(parser, input, e))
      case Failure(e) => scheme.failure(e)
    }
  }

  def parseInputDocument(
      input: String,
      config: ParserConfig = ParserConfig.default
  )(implicit scheme: DeliveryScheme[ast.InputDocument]): scheme.Result =
    parseInputDocument(ParserInput(input), config)(scheme)

  def parseInputDocument(input: ParserInput, config: ParserConfig)(
      implicit scheme: DeliveryScheme[ast.InputDocument]
  ): scheme.Result = {
    val id = config.sourceIdFn(input)
    val parser = new QueryParser(
      input,
      id,
      config.legacyImplementsInterface,
      config.legacyEmptyFields,
      config.experimentalFragmentVariables,
      config.parseLocations,
      config.parseComments
    )

    parser.InputDocument.run() match {
      case Success(res) =>
        scheme.success(
          res.copy(sourceMapper = config.sourceMapperFn(id, input))
        )
      case Failure(e: ParseError) =>
        scheme.failure(SyntaxError(parser, input, e))
      case Failure(e) => scheme.failure(e)
    }
  }

  def parseInputWithVariables(
      input: String
  )(implicit scheme: DeliveryScheme[ast.Value]): scheme.Result =
    parseInputWithVariables(ParserInput(input))(scheme)

  def parseInputWithVariables(
      input: ParserInput
  )(implicit scheme: DeliveryScheme[ast.Value]): scheme.Result = {
    val parser = new QueryParser(input, "")

    parser.InputDocumentWithVariables.run() match {
      case Success(res) if res.values.nonEmpty =>
        scheme.success(res.values.head)
      case Success(res) =>
        scheme.failure(
          new IllegalArgumentException(
            "Input document does not contain any value definitions."
          )
        )
      case Failure(e: ParseError) =>
        scheme.failure(SyntaxError(parser, input, e))
      case Failure(e) => scheme.failure(e)
    }
  }

  def parseInputDocumentWithVariables(
      input: String,
      config: ParserConfig = ParserConfig.default
  )(implicit scheme: DeliveryScheme[ast.InputDocument]): scheme.Result =
    parseInputDocumentWithVariables(ParserInput(input), config)(scheme)

  def parseInputDocumentWithVariables(input: ParserInput, config: ParserConfig)(
      implicit scheme: DeliveryScheme[ast.InputDocument]
  ): scheme.Result = {
    val id = config.sourceIdFn(input)
    val parser = new QueryParser(input, id)

    parser.InputDocumentWithVariables.run() match {
      case Success(res) =>
        scheme.success(
          res.copy(sourceMapper = config.sourceMapperFn(id, input))
        )
      case Failure(e: ParseError) =>
        scheme.failure(SyntaxError(parser, input, e))
      case Failure(e) => scheme.failure(e)
    }
  }
}
