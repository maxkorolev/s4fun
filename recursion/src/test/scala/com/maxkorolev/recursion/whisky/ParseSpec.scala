package com.maxkorolev.recursion.whisky

import cats.effect._
import org.scalatest._

import scala.concurrent.ExecutionContext
import scala.annotation.tailrec
import com.maxkorolev.recursion.whisky.parser.QueryParser
import com.maxkorolev.recursion.whisky.ast._

import higherkindness.droste._

class ParseSpec extends FlatSpec with Matchers {

  "Parse" should "work" in {

    val query =
      """
        query FetchLukeAndLeiaAliased(
              $someVar: Int = 1.23
              $anotherVar: Int = 123) @include(if: true) {
          luke: human(id: "1000")@include(if: true){
            friends(sort: NAME)
          }

          leia: human(id: "10103\n \u00F6 ö") {
            name
          }

          ... on User {
            birth{day}
          }

          ...Foo
        }

        fragment Foo on User @foo(bar: 1) {
          baz
        }
  """

    val evaluateAlgebra: Algebra[AstNode, String] = Algebra {

      case Field(
          alias,
          name,
          arguments,
          directives,
          selections,
          comments,
          trailingComments
          ) =>
        s"${alias getOrElse ""} $name(${arguments.mkString(", ")} @$di)"
      case FragmentSpread(name, directives, comments) =>
      case InlineFragment(
          typeCondition,
          directives,
          selections,
          comments,
          trailingComments
          )                                                  =>
      case ObjectField(name, value, comments)                =>
      case OperationTypeDefinition(operation, tpe, comments) =>
      case InputValueDefinition(
          name,
          valueType,
          defaultValue,
          directives,
          description,
          comments
          )                                                             =>
      case EnumValueDefinition(name, directives, description, comments) =>
      case SchemaExtensionDefinition(
          operationTypes,
          directives,
          comments,
          trailingComments
          )                                                          =>
      case ScalarTypeExtensionDefinition(name, directives, comments) =>
      case InputObjectTypeExtensionDefinition(
          name,
          fields,
          directives,
          comments,
          trailingComments
          )                                                                =>
      case UnionTypeExtensionDefinition(name, types, directives, comments) =>
      case InterfaceTypeExtensionDefinition(
          name,
          fields,
          directives,
          comments,
          trailingComments
          ) =>
      case ObjectTypeExtensionDefinition(
          name,
          interfaces,
          fields,
          directives,
          comments,
          trailingComments
          ) =>
      case EnumTypeExtensionDefinition(
          name,
          values,
          directives,
          comments,
          trailingComments
          ) =>
      case FieldDefinition(
          name,
          fieldType,
          arguments,
          directives,
          description,
          comments
          ) =>
      case DirectiveDefinition(
          name,
          arguments,
          locations,
          description,
          comments
          ) =>
      case SchemaDefinition(
          operationTypes,
          directives,
          description,
          comments,
          trailingComments
          ) =>
      case InputObjectTypeDefinition(
          name,
          fields,
          directives,
          description,
          comments,
          trailingComments
          ) =>
      case EnumTypeDefinition(
          name,
          values,
          directives,
          description,
          comments,
          trailingComments
          ) =>
      case ObjectTypeDefinition(
          name,
          interfaces,
          fields,
          directives,
          description,
          comments,
          trailingComments
          )                                                              =>
      case ScalarTypeDefinition(name, directives, description, comments) =>
      case InterfaceTypeDefinition(
          name,
          fields,
          directives,
          description,
          comments,
          trailingComments
          ) =>
      case UnionTypeDefinition(
          name,
          types,
          directives,
          description,
          comments
          )                                                                  =>
      case DirectiveLocation(name, comments)                                 =>
      case Document(definitions, trailingComments)                           =>
      case FloatValue(value, comments)                                       =>
      case StringValue(value, block, blockRawValue, comments)                =>
      case IntValue(value, comments)                                         =>
      case BooleanValue(value, comments)                                     =>
      case BigIntValue(value, comments)                                      =>
      case BigDecimalValue(value, comments)                                  =>
      case EnumValue(value, comments)                                        =>
      case ObjectValue(fields, comments)                                     =>
      case VariableValue(name, comments)                                     =>
      case NullValue(comments)                                               =>
      case ListValue(values, comments)                                       =>
      case Directive(name, arguments, comments)                              =>
      case VariableDefinition(name, tpe, defaultValue, directives, comments) =>
      case Comment(text)                                                     =>
      case SelectionSet(selections, comments)                                =>
      case Argument(name, value, comments)                                   =>
      case ListType(ofType)                                                  =>
      case NamedType(name)                                                   =>
      case NotNullType(ofType)                                               =>
      case FragmentDefinition(
          name,
          typeCondition,
          directives,
          selections,
          variables,
          comments,
          trailingComments
          ) =>
      case OperationDefinition(
          operationType,
          name,
          variables,
          directives,
          selections,
          comments,
          trailingComments
          )                                        =>
      case InputDocument(values, trailingComments) =>
    }

    val evaluate: RecursiveExpr => BigDecimal = scheme.cata(evaluateAlgebra)

    val ast = QueryParser.parse(query).map { ast =>
    }

    ast shouldBe true

  }
}
