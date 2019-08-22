package com.maxkorolev.recursion.whisky.parser

import java.util.UUID

import org.parboiled2._
import org.parboiled2.CharPredicate.{Digit19, HexDigit}

import scala.util.{Failure, Success}
import com.maxkorolev.recursion.whisky.ast

trait Fragments {
  this: Parser
    with Tokens
    with Ignored
    with Directives
    with Types
    with Operations =>

  def experimentalFragmentVariables: Boolean

  def FragmentSpread = rule {
    Comments ~ trackPos ~ Ellipsis ~ FragmentName ~ (Directives.? ~> (_ getOrElse Nil)) ~> {
      (comment, location, name, dirs) =>
        ast.Ast(location, ast.FragmentSpread(name, dirs, comment))
    }
  }

  def InlineFragment = rule {
    Comments ~ trackPos ~ Ellipsis ~ TypeCondition.? ~ (Directives.? ~> (_ getOrElse Nil)) ~ SelectionSet ~> {
      (comment, location, typeCondition, dirs, sels) =>
        ast.Ast(
          location,
          ast.InlineFragment(
            typeCondition,
            dirs,
            sels.selections,
            comment,
            sels.comments
          )
        )
    }
  }

  def on = rule { Keyword("on") }

  def Fragment = rule { Keyword("fragment") }

  def FragmentDefinition = rule {
    Comments ~ trackPos ~ Fragment ~ FragmentName ~ ExperimentalFragmentVariables ~ TypeCondition ~ (Directives.? ~> (_ getOrElse Nil)) ~ SelectionSet ~> {
      (comment, location, name, vars, typeCondition, dirs, sels) =>
        ast.Ast(
          location,
          ast.FragmentDefinition(
            name,
            typeCondition,
            dirs,
            sels._1,
            vars,
            comment,
            sels._2
          )
        )
    }
  }

  def ExperimentalFragmentVariables = rule {
    test(experimentalFragmentVariables) ~ VariableDefinitions.? ~> (_ getOrElse Nil) | push(
      Nil
    )
  }

  def FragmentName = rule { !on ~ Name }

  def TypeCondition = rule { on ~ NamedType }

}
