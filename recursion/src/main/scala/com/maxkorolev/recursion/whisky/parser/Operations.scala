package com.maxkorolev.recursion.whisky.parser

import java.util.UUID

import org.parboiled2._
import org.parboiled2.CharPredicate.{Digit19, HexDigit}

import scala.util.{Failure, Success}
import com.maxkorolev.recursion.whisky.ast

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
      ast.Ast(
        location,
        ast.OperationDefinition(
          selections = s._1,
          comments = comment,
          trailingComments = s._2
        )
      )
    } |
      Comments ~ trackPos ~ OperationType ~ OperationName.? ~ (VariableDefinitions.? ~> (_ getOrElse Nil)) ~ (Directives.? ~> (_ getOrElse Nil)) ~ SelectionSet ~> {
        (comment, location, opType, name, vars, dirs, sels) =>
          ast.Ast(
            location,
            ast.OperationDefinition(
              opType,
              name,
              vars,
              dirs,
              sels._1,
              comment,
              sels._2
            )
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
    wsNoComment('(') ~ VariableDefinition.+ ~ wsNoComment(')') ~> (_.toList)
  }

  def VariableDefinition = rule {
    Comments ~ trackPos ~ Variable ~ ws(':') ~ Type ~ DefaultValue.? ~ (DirectivesConst.? ~> (_ getOrElse Nil)) ~> {
      (comment, location, name, tpe, defaultValue, dirs) =>
        ast.Ast(
          location,
          ast.VariableDefinition(
            name,
            tpe,
            defaultValue,
            dirs,
            comment
          )
        )
    }
  }

  def Variable = rule { Ignored.* ~ '$' ~ NameStrict }

  def DefaultValue = rule { wsNoComment('=') ~ ValueConst }

  def SelectionSet: Rule1[(List[ast.Ast], List[ast.Ast])] = rule {
    wsNoComment('{') ~ Selection.+ ~ Comments ~ wsNoComment('}') ~> {
      (x: Seq[ast.Ast], comments: List[ast.Ast]) =>
        x.toList -> comments
    }
  }

  def Selection = rule { Field | FragmentSpread | InlineFragment }

  def Field = rule {
    Comments ~ trackPos ~ Alias.? ~ Name ~
      (Arguments.? ~> (_ getOrElse Nil)) ~
      (Directives.? ~> (_ getOrElse Nil)) ~
      (SelectionSet.? ~> (_ getOrElse (Nil, Nil))) ~> {
      (comment, location, alias, name, args, dirs, sels) =>
        ast.Ast(
          location,
          ast.Field(alias, name, args, dirs, sels._1, comment, sels._2)
        )
    }
  }

  def Alias = rule { Name ~ ws(':') }

  def Arguments = rule {
    Ignored.* ~ wsNoComment('(') ~ Argument.+ ~ wsNoComment(')') ~> (_.toList)
  }

  def ArgumentsConst = rule {
    Ignored.* ~ wsNoComment('(') ~ ArgumentConst.+ ~ wsNoComment(')') ~> (_.toList)
  }

  def Argument = rule {
    Comments ~ trackPos ~ Name ~ wsNoComment(':') ~ Value ~> {
      (comment, location, name, value) =>
        ast.Ast(location, ast.Argument(name, value, comment))
    }
  }

  def ArgumentConst = rule {
    Comments ~ trackPos ~ Name ~ wsNoComment(':') ~ ValueConst ~> {
      (comment, location, name, value) =>
        ast.Ast(location, ast.Argument(name, value, comment))
    }
  }

}
