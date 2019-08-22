package com.maxkorolev.recursion.whisky.parser

import java.util.UUID

import org.parboiled2._
import org.parboiled2.CharPredicate.{Digit19, HexDigit}

import scala.util.{Failure, Success}
import com.maxkorolev.recursion.whisky.ast
import higherkindness.droste._

trait Document {
  this: Parser
    with Operations
    with Ignored
    with Fragments
    with Operations
    with Values
    with TypeSystemDefinitions =>

  def Document = rule {
    IgnoredNoComment.* ~ trackPos ~ Definition.+ ~ IgnoredNoComment.* ~ Comments ~ EOI ~> {
      (location, d, comments) =>
        ast.Ast(location, ast.Document(d.toList, comments))
    }
  }

  def InputDocument = rule {
    IgnoredNoComment.* ~ trackPos ~ ValueConst.+ ~ IgnoredNoComment.* ~ Comments ~ EOI ~> {
      (location, vs, comments) =>
        ast.Ast(location, ast.InputDocument(vs.toList, comments))
    }
  }

  def InputDocumentWithVariables = rule {
    IgnoredNoComment.* ~ trackPos ~ Value.+ ~ IgnoredNoComment.* ~ Comments ~ EOI ~> {
      (location, vs, comments) =>
        ast.Ast(location, ast.InputDocument(vs.toList, comments))
    }
  }

  def Definition = rule {
    ExecutableDefinition | TypeSystemDefinition | TypeSystemExtension
  }

  def ExecutableDefinition = rule { OperationDefinition | FragmentDefinition }

}
