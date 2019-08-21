package com.maxkorolev.recursion.whisky.parser

import java.util.UUID

import org.parboiled2._
import org.parboiled2.CharPredicate.{Digit19, HexDigit}

import scala.util.{Failure, Success}

trait Fragments {
  this: Parser
    with Tokens
    with Ignored
    with Directives
    with Types
    with Operations =>

  def experimentalFragmentVariables: Boolean

  def FragmentSpread = rule {
    Comments ~ trackPos ~ Ellipsis ~ FragmentName ~ (Directives.? ~> (_ getOrElse Vector.empty)) ~> {
      (comment, location, name, dirs) =>
        ast.FragmentSpread(name, dirs, comment, location)
    }
  }

  def InlineFragment = rule {
    Comments ~ trackPos ~ Ellipsis ~ TypeCondition.? ~ (Directives.? ~> (_ getOrElse Vector.empty)) ~ SelectionSet ~> {
      (comment, location, typeCondition, dirs, sels) =>
        ast.InlineFragment(
          typeCondition,
          dirs,
          sels._1,
          comment,
          sels._2,
          location
        )
    }
  }

  def on = rule { Keyword("on") }

  def Fragment = rule { Keyword("fragment") }

  def FragmentDefinition = rule {
    Comments ~ trackPos ~ Fragment ~ FragmentName ~ ExperimentalFragmentVariables ~ TypeCondition ~ (Directives.? ~> (_ getOrElse Vector.empty)) ~ SelectionSet ~> {
      (comment, location, name, vars, typeCondition, dirs, sels) =>
        ast.FragmentDefinition(
          name,
          typeCondition,
          dirs,
          sels._1,
          vars,
          comment,
          sels._2,
          location
        )
    }
  }

  def ExperimentalFragmentVariables = rule {
    test(experimentalFragmentVariables) ~ VariableDefinitions.? ~> (_ getOrElse Vector.empty) | push(
      Vector.empty
    )
  }

  def FragmentName = rule { !on ~ Name }

  def TypeCondition = rule { on ~ NamedType }

}
