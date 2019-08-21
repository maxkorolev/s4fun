package com.maxkorolev.recursion.whisky.parser

import java.util.UUID

import org.parboiled2._
import org.parboiled2.CharPredicate.{Digit19, HexDigit}

import scala.util.{Failure, Success}

case class ParserConfig(
    experimentalFragmentVariables: Boolean = false,
    sourceIdFn: ParserInput => String = ParserConfig.defaultSourceIdFn,
    sourceMapperFn: (String, ParserInput) => Option[SourceMapper] =
      ParserConfig.defaultSourceMapperFn,
    parseLocations: Boolean = true,
    parseComments: Boolean = true
) {

  def withExperimentalFragmentVariables: ParserConfig =
    copy(experimentalFragmentVariables = true)

  def withEmptySourceId: ParserConfig =
    copy(sourceIdFn = ParserConfig.emptySourceIdFn)

  def withSourceMapper(
      fn: (String, ParserInput) => Option[SourceMapper]
  ): ParserConfig = copy(sourceMapperFn = fn)

  def withoutSourceMapper: ParserConfig =
    copy(sourceMapperFn = ParserConfig.emptySourceMapperFn)

  def withoutLocations: ParserConfig = copy(parseLocations = false)

  def withoutComments: ParserConfig = copy(parseComments = false)
}

object ParserConfig {
  lazy val default: ParserConfig = ParserConfig()

  lazy val emptySourceIdFn: ParserInput => String = _ => ""
  lazy val defaultSourceIdFn: ParserInput => String = _ =>
    UUID.randomUUID().toString

  lazy val emptySourceMapperFn: (String, ParserInput) => Option[SourceMapper] =
    (_, _) => None
  lazy val defaultSourceMapperFn
      : (String, ParserInput) => Option[SourceMapper] =
    (id, input) => Some(new DefaultSourceMapper(id, input))
}
