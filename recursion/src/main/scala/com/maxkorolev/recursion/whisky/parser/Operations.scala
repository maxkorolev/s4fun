package com.maxkorolev.recursion.whisky.parser

import java.util.UUID

import org.parboiled2._
import org.parboiled2.CharPredicate.{Digit19, HexDigit}

import scala.util.{Failure, Success}

trait Operations extends PositionTracking {
  this: Parser
    with Tokens
    with Ignored
    with Fragments
    with Values
    with Types
    with Directives =>

  def OperationDefinition = rule {
    Comments ~ trackPos ~ SelectionSet ~> { (comment, location, s) =>
      ast.OperationDefinition(
        selections = s._1,
        comments = comment,
        trailingComments = s._2,
        location = location
      )
    } |
      Comments ~ trackPos ~ OperationType ~ OperationName.? ~ (VariableDefinitions.? ~> (_ getOrElse Vector.empty)) ~ (Directives.? ~> (_ getOrElse Vector.empty)) ~ SelectionSet ~> {
        (comment, location, opType, name, vars, dirs, sels) =>
          ast.OperationDefinition(
            opType,
            name,
            vars,
            dirs,
            sels._1,
            comment,
            sels._2,
            location
          )
      }
  }

  def OperationName = rule { Name }

  def OperationType = rule {
    Query ~ push(ast.OperationType.Query) |
      Mutation ~ push(ast.OperationType.Mutation) |
      Subscription ~ push(ast.OperationType.Subscription)
  }

  def Query = rule { Keyword("query") }

  def Mutation = rule { Keyword("mutation") }

  def Subscription = rule { Keyword("subscription") }

  def VariableDefinitions = rule {
    wsNoComment('(') ~ VariableDefinition.+ ~ wsNoComment(')') ~> (_.toVector)
  }

  def VariableDefinition = rule {
    Comments ~ trackPos ~ Variable ~ ws(':') ~ Type ~ DefaultValue.? ~ (DirectivesConst.? ~> (_ getOrElse Vector.empty)) ~> {
      (comment, location, name, tpe, defaultValue, dirs) =>
        ast.VariableDefinition(name, tpe, defaultValue, dirs, comment, location)
    }
  }

  def Variable = rule { Ignored.* ~ '$' ~ NameStrict }

  def DefaultValue = rule { wsNoComment('=') ~ ValueConst }

  def SelectionSet: Rule1[(Vector[ast.Selection], Vector[ast.Comment])] = rule {
    wsNoComment('{') ~ Selection.+ ~ Comments ~ wsNoComment('}') ~> {
      (x: Seq[ast.Selection], comments: Vector[ast.Comment]) =>
        x.toVector → comments
    }
  }

  def Selection = rule { Field | FragmentSpread | InlineFragment }

  def Field = rule {
    Comments ~ trackPos ~ Alias.? ~ Name ~
      (Arguments.? ~> (_ getOrElse Vector.empty)) ~
      (Directives.? ~> (_ getOrElse Vector.empty)) ~
      (SelectionSet.? ~> (_ getOrElse (Vector.empty → Vector.empty))) ~> {
      (comment, location, alias, name, args, dirs, sels) =>
        ast.Field(alias, name, args, dirs, sels._1, comment, sels._2, location)
    }
  }

  def Alias = rule { Name ~ ws(':') }

  def Arguments = rule {
    Ignored.* ~ wsNoComment('(') ~ Argument.+ ~ wsNoComment(')') ~> (_.toVector)
  }

  def ArgumentsConst = rule {
    Ignored.* ~ wsNoComment('(') ~ ArgumentConst.+ ~ wsNoComment(')') ~> (_.toVector)
  }

  def Argument = rule {
    Comments ~ trackPos ~ Name ~ wsNoComment(':') ~ Value ~> {
      (comment, location, name, value) =>
        ast.Argument(name, value, comment, location)
    }
  }

  def ArgumentConst = rule {
    Comments ~ trackPos ~ Name ~ wsNoComment(':') ~ ValueConst ~> {
      (comment, location, name, value) =>
        ast.Argument(name, value, comment, location)
    }
  }

}
