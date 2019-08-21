package com.maxkorolev.recursion.whisky.parser

import java.util.UUID

import org.parboiled2._
import org.parboiled2.CharPredicate.{Digit19, HexDigit}

import scala.util.{Failure, Success}

trait Document {
  this: Parser
    with Operations
    with Ignored
    with Fragments
    with Operations
    with Values
    with TypeSystemDefinitions =>

  def Document = rule {
    IgnoredNoComment.* ~ trackPos ~ Definition.+ ~ IgnoredNoComment.* ~ Comments ~ EOI ~>
      ((location, d, comments) => ast.Document(d.toVector, comments, location))
  }

  def InputDocument = rule {
    IgnoredNoComment.* ~ trackPos ~ ValueConst.+ ~ IgnoredNoComment.* ~ Comments ~ EOI ~> {
      (location, vs, comments) =>
        ast.InputDocument(vs.toVector, comments, location)
    }
  }

  def InputDocumentWithVariables = rule {
    IgnoredNoComment.* ~ trackPos ~ Value.+ ~ IgnoredNoComment.* ~ Comments ~ EOI ~> {
      (location, vs, comments) =>
        ast.InputDocument(vs.toVector, comments, location)
    }
  }

  def Definition = rule {
    ExecutableDefinition |
      TypeSystemDefinition |
      TypeSystemExtension
  }

  def ExecutableDefinition = rule {
    OperationDefinition |
      FragmentDefinition
  }

}
