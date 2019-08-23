package com.maxkorolev.recursion.whisky.parser

import java.util.UUID

import org.parboiled2._
import org.parboiled2.CharPredicate.{Digit19, HexDigit}
import com.maxkorolev.recursion.whisky.ast

import scala.util.{Failure, Success}

trait TypeSystemDefinitions {
  this: Parser
    with Tokens
    with Ignored
    with Directives
    with Types
    with Operations
    with Values
    with Fragments =>

  def scalar = rule { Keyword("scalar") }
  def `type` = rule { Keyword("type") }
  def interface = rule { Keyword("interface") }
  def union = rule { Keyword("union") }
  def enum = rule { Keyword("enum") }
  def inputType = rule { Keyword("input") }
  def implements = rule { Keyword("implements") }
  def extend = rule { Keyword("extend") }
  def directive = rule { Keyword("directive") }
  def schema = rule { Keyword("schema") }

  def TypeSystemDefinition = rule {
    SchemaDefinition |
      TypeDefinition |
      DirectiveDefinition
  }

  def TypeDefinition = rule {
    ScalarTypeDefinition |
      ObjectTypeDefinition |
      InterfaceTypeDefinition |
      UnionTypeDefinition |
      EnumTypeDefinition |
      InputObjectTypeDefinition
  }

  def ScalarTypeDefinition = rule {
    Description ~ Comments ~ trackPos ~ scalar ~ Name ~ (DirectivesConst.? ~> (_ getOrElse Nil)) ~> {
      (descr, comment, location, name, dirs) =>
        ast.Ast(location, ast.ScalarTypeDefinition(name, dirs, descr, comment))
    }
  }

  def ObjectTypeDefinition = rule {
    Description ~ Comments ~ trackPos ~ `type` ~ Name ~ (ImplementsInterfaces.? ~> (_ getOrElse Nil)) ~ (DirectivesConst.? ~> (_ getOrElse Nil)) ~ FieldsDefinition.? ~> {
      (descr, comment, location, name, interfaces, dirs, fields) =>
        ast.Ast(
          location,
          ast.ObjectTypeDefinition(
            name,
            interfaces,
            fields.fold(Nil: List[ast.Ast])(_._1.toList),
            dirs,
            descr,
            comment,
            fields.fold(Nil: List[ast.Ast])(_._2)
          )
        )
    }
  }

  def TypeSystemExtension = rule {
    SchemaExtension |
      TypeExtension
  }

  def TypeExtension = rule {
    ScalarTypeExtensionDefinition |
      ObjectTypeExtensionDefinition |
      InterfaceTypeExtensionDefinition |
      UnionTypeExtensionDefinition |
      EnumTypeExtensionDefinition |
      InputObjectTypeExtensionDefinition
  }

  def SchemaExtension = rule {
    (Comments ~ trackPos ~ extend ~ schema ~ (DirectivesConst.? ~> (_ getOrElse Nil)) ~ wsNoComment(
      '{'
    ) ~ OperationTypeDefinition.+ ~ Comments ~ wsNoComment('}') ~> {
      (comment, location, dirs, ops, tc) =>
        ast.Ast(
          location,
          ast.SchemaExtensionDefinition(ops.toList, dirs, comment, tc)
        )
    }) |
      (Comments ~ trackPos ~ extend ~ schema ~ DirectivesConst ~> {
        (comment, location, dirs) =>
          ast.Ast(
            location,
            ast.SchemaExtensionDefinition(
              Nil,
              dirs,
              comment,
              Nil
            )
          )
      })
  }

  def ObjectTypeExtensionDefinition = rule {
    (Comments ~ trackPos ~ extend ~ `type` ~ Name ~ (ImplementsInterfaces.? ~> (_ getOrElse Nil)) ~ (DirectivesConst.? ~> (_ getOrElse Nil)) ~ FieldsDefinition ~> {
      (comment, location, name, interfaces, dirs, fields) =>
        ast.Ast(
          location,
          ast.ObjectTypeExtensionDefinition(
            name,
            interfaces,
            fields._1.toList,
            dirs,
            comment,
            fields._2
          )
        )
    }) |
      (Comments ~ trackPos ~ extend ~ `type` ~ Name ~ (ImplementsInterfaces.? ~> (_ getOrElse Nil)) ~ DirectivesConst ~> {
        (comment, location, name, interfaces, dirs) =>
          ast.Ast(
            location,
            ast.ObjectTypeExtensionDefinition(
              name,
              interfaces,
              Nil,
              dirs,
              comment,
              Nil
            )
          )
      }) |
      (Comments ~ trackPos ~ extend ~ `type` ~ Name ~ ImplementsInterfaces ~> {
        (comment, location, name, interfaces) =>
          ast.Ast(
            location,
            ast.ObjectTypeExtensionDefinition(
              name,
              interfaces,
              Nil,
              Nil,
              comment,
              Nil
            )
          )
      })
  }

  def InterfaceTypeExtensionDefinition = rule {
    (Comments ~ trackPos ~ extend ~ interface ~ Name ~ (DirectivesConst.? ~> (_ getOrElse Nil)) ~ FieldsDefinition ~> {
      (comment, location, name, dirs, fields) =>
        ast.Ast(
          location,
          ast.InterfaceTypeExtensionDefinition(
            name,
            fields._1.toList,
            dirs,
            comment,
            fields._2
          )
        )
    }) |
      (Comments ~ trackPos ~ extend ~ interface ~ Name ~ DirectivesConst ~> {
        (comment, location, name, dirs) =>
          ast.Ast(
            location,
            ast.InterfaceTypeExtensionDefinition(
              name,
              Nil,
              dirs,
              comment,
              Nil
            )
          )
      })
  }

  def UnionTypeExtensionDefinition = rule {
    (Comments ~ trackPos ~ extend ~ union ~ Name ~ (DirectivesConst.? ~> (_ getOrElse Nil)) ~ UnionMemberTypes ~> {
      (comment, location, name, dirs, types) =>
        ast.Ast(
          location,
          ast.UnionTypeExtensionDefinition(name, types, dirs, comment)
        )
    }) |
      (Comments ~ trackPos ~ extend ~ union ~ Name ~ DirectivesConst ~> {
        (comment, location, name, dirs) =>
          ast.Ast(
            location,
            ast.UnionTypeExtensionDefinition(
              name,
              Nil,
              dirs,
              comment
            )
          )
      })
  }

  def EnumTypeExtensionDefinition = rule {
    (Comments ~ trackPos ~ extend ~ enum ~ Name ~ (DirectivesConst.? ~> (_ getOrElse Nil)) ~ EnumValuesDefinition ~> {
      (comment, location, name, dirs, values) =>
        ast.Ast(
          location,
          ast.EnumTypeExtensionDefinition(
            name,
            values._1.toList,
            dirs,
            comment,
            values._2
          )
        )
    }) |
      (Comments ~ trackPos ~ extend ~ enum ~ Name ~ DirectivesConst ~> {
        (comment, location, name, dirs) =>
          ast.Ast(
            location,
            ast.EnumTypeExtensionDefinition(
              name,
              Nil,
              dirs,
              comment,
              Nil
            )
          )
      })
  }

  def InputObjectTypeExtensionDefinition = rule {
    (Comments ~ trackPos ~ extend ~ inputType ~ Name ~ (DirectivesConst.? ~> (_ getOrElse Nil)) ~ InputFieldsDefinition ~> {
      (comment, location, name, dirs, fields) =>
        ast.Ast(
          location,
          ast.InputObjectTypeExtensionDefinition(
            name,
            fields._1.toList,
            dirs,
            comment,
            fields._2
          )
        )
    }) |
      (Comments ~ trackPos ~ extend ~ inputType ~ Name ~ DirectivesConst ~> {
        (comment, location, name, dirs) =>
          ast.Ast(
            location,
            ast.InputObjectTypeExtensionDefinition(
              name,
              Nil,
              dirs,
              comment,
              Nil
            )
          )
      })
  }

  def ScalarTypeExtensionDefinition = rule {
    (Comments ~ trackPos ~ extend ~ scalar ~ Name ~ DirectivesConst ~> {
      (comment, location, name, dirs) =>
        ast
          .Ast(location, ast.ScalarTypeExtensionDefinition(name, dirs, comment))
    })
  }

  def ImplementsInterfaces = rule {
    implements ~ ws('&').? ~ NamedType.+(ws('&')) ~> (_.toList)
  }

  def FieldsDefinition = rule {
    wsNoComment('{') ~ FieldDefinition.+ ~ Comments ~ wsNoComment('}') ~> (_ -> _)
  }
  def FieldDefinition = rule {
    Description ~ Comments ~ trackPos ~ Name ~ (ArgumentsDefinition.? ~> (_ getOrElse Nil)) ~ ws(
      ':'
    ) ~ Type ~ (Directives.? ~> (_ getOrElse Nil)) ~> {
      (descr, comment, location, name, args, fieldType, dirs) =>
        ast.Ast(
          location,
          ast.FieldDefinition(
            name,
            fieldType,
            args,
            dirs,
            descr,
            comment
          )
        )
    }
  }

  def ArgumentsDefinition = rule {
    wsNoComment('(') ~ InputValueDefinition.+ ~ wsNoComment(')') ~> (_.toList)
  }

  def InputValueDefinition = rule {
    Description ~ Comments ~ trackPos ~ Name ~ ws(':') ~ Type ~ DefaultValue.? ~ (DirectivesConst.? ~> (_ getOrElse Nil)) ~> {
      (descr, comment, location, name, valueType, default, dirs) =>
        ast.Ast(
          location,
          ast.InputValueDefinition(
            name,
            valueType,
            default,
            dirs,
            descr,
            comment
          )
        )
    }
  }

  def InterfaceTypeDefinition = rule {
    Description ~ Comments ~ trackPos ~ interface ~ Name ~ (DirectivesConst.? ~> (_ getOrElse Nil)) ~ FieldsDefinition.? ~> {
      (descr, comment, location, name, dirs, fields) =>
        ast.Ast(
          location,
          ast.InterfaceTypeDefinition(
            name,
            fields.fold(Nil: List[ast.Ast])(_._1.toList),
            dirs,
            descr,
            comment,
            fields.fold(Nil: List[ast.Ast])(_._2)
          )
        )
    }
  }

  def UnionTypeDefinition = rule {
    Description ~ Comments ~ trackPos ~ union ~ Name ~ (DirectivesConst.? ~> (_ getOrElse Nil)) ~ (UnionMemberTypes.? ~> (_ getOrElse Nil)) ~> {
      (descr, comment, location, name, dirs, members) =>
        ast.Ast(
          location,
          ast.UnionTypeDefinition(name, members, dirs, descr, comment)
        )
    }
  }

  def UnionMemberTypes = rule { wsNoComment('=') ~ UnionMembers }

  def UnionMembers = rule { ws('|').? ~ NamedType.+(ws('|')) ~> (_.toList) }

  def EnumTypeDefinition = rule {
    Description ~ Comments ~ trackPos ~ enum ~ Name ~ (DirectivesConst.? ~> (_ getOrElse Nil)) ~ EnumValuesDefinition.? ~> {
      (descr, comment, location, name, dirs, values) =>
        ast.Ast(
          location,
          ast.EnumTypeDefinition(
            name,
            values.fold(Nil: List[ast.Ast])(_._1.toList),
            dirs,
            descr,
            comment,
            values.fold(Nil: List[ast.Ast])(_._2)
          )
        )
    }
  }

  def EnumValuesDefinition = rule {
    wsNoComment('{') ~ EnumValueDefinition.+ ~ Comments ~ wsNoComment('}') ~> (_ -> _)
  }

  def EnumValueDefinition = rule {
    Description ~ Comments ~ trackPos ~ Name ~ (DirectivesConst.? ~> (_ getOrElse Nil)) ~> {
      (descr, comments, location, name, dirs) =>
        ast.Ast(location, ast.EnumValueDefinition(name, dirs, descr, comments))
    }
  }

  def InputObjectTypeDefinition = rule {
    Description ~ Comments ~ trackPos ~ inputType ~ Name ~ (DirectivesConst.? ~> (_ getOrElse Nil)) ~ InputFieldsDefinition.? ~> {
      (descr, comment, location, name, dirs, fields) =>
        ast.Ast(
          location,
          ast.InputObjectTypeDefinition(
            name,
            fields.fold(Nil: List[ast.Ast])(_._1.toList),
            dirs,
            descr,
            comment,
            fields.fold(Nil: List[ast.Ast])(_._2)
          )
        )
    }
  }

  def InputFieldsDefinition = rule {
    wsNoComment('{') ~ InputValueDefinition.+ ~ Comments ~ wsNoComment('}') ~> (_ -> _)
  }

  def DirectiveDefinition = rule {
    Description ~ Comments ~ trackPos ~ directive ~ '@' ~ NameStrict ~ (ArgumentsDefinition.? ~> (_ getOrElse Nil)) ~ on ~ DirectiveLocations ~> {
      (descr, comment, location, name, args, locations) =>
        ast.Ast(
          location,
          ast.DirectiveDefinition(
            name,
            args,
            locations,
            descr,
            comment
          )
        )
    }
  }

  def DirectiveLocations = rule {
    ws('|').? ~ DirectiveLocation.+(wsNoComment('|')) ~> (_.toList)
  }

  def DirectiveLocation = rule {
    Comments ~ trackPos ~ DirectiveLocationName ~> {
      (comment, location, name) =>
        ast.Ast(location, ast.DirectiveLocation(name, comment))
    }
  }

  def DirectiveLocationName = rule {
    TypeSystemDirectiveLocation | ExecutableDirectiveLocation
  }

  def ExecutableDirectiveLocation = rule {
    wsCapture("QUERY") |
      wsCapture("MUTATION") |
      wsCapture("SUBSCRIPTION") |
      wsCapture("FIELD") |
      wsCapture("FRAGMENT_DEFINITION") |
      wsCapture("FRAGMENT_SPREAD") |
      wsCapture("INLINE_FRAGMENT")
  }

  def TypeSystemDirectiveLocation = rule {
    wsCapture("SCHEMA") |
      wsCapture("SCALAR") |
      wsCapture("OBJECT") |
      wsCapture("FIELD_DEFINITION") |
      wsCapture("ARGUMENT_DEFINITION") |
      wsCapture("INTERFACE") |
      wsCapture("UNION") |
      wsCapture("ENUM_VALUE") |
      wsCapture("ENUM") |
      wsCapture("INPUT_OBJECT") |
      wsCapture("INPUT_FIELD_DEFINITION") |
      wsCapture("VARIABLE_DEFINITION")
  }

  def SchemaDefinition = rule {
    Description ~ Comments ~ trackPos ~ schema ~ (DirectivesConst.? ~> (_ getOrElse Nil)) ~ wsNoComment(
      '{'
    ) ~ OperationTypeDefinition.+ ~ Comments ~ wsNoComment('}') ~> {
      (descr, comment, location, dirs, ops, tc) =>
        ast.Ast(
          location,
          ast.SchemaDefinition(ops.toList, dirs, descr, comment, tc)
        )
    }
  }

  def OperationTypeDefinition = rule {
    Comments ~ trackPos ~ OperationType ~ ws(':') ~ NamedType ~> {
      (comment, location, opType, tpe) =>
        ast.Ast(location, ast.OperationTypeDefinition(opType, tpe, comment))
    }
  }

  def Description = rule { StringValue.? }

}
