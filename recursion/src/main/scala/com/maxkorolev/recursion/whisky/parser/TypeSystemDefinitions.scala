package com.maxkorolev.recursion.whisky.parser

import java.util.UUID

import org.parboiled2._
import org.parboiled2.CharPredicate.{Digit19, HexDigit}

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
  def legacyImplementsInterface: Boolean
  def legacyEmptyFields: Boolean

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
    Description ~ Comments ~ trackPos ~ scalar ~ Name ~ (DirectivesConst.? ~> (_ getOrElse Vector.empty)) ~> {
      (descr, comment, location, name, dirs) =>
        ast.ScalarTypeDefinition(name, dirs, descr, comment, location)
    }
  }

  def ObjectTypeDefinition = rule {
    Description ~ Comments ~ trackPos ~ `type` ~ Name ~ (ImplementsInterfaces.? ~> (_ getOrElse Vector.empty)) ~ (DirectivesConst.? ~> (_ getOrElse Vector.empty)) ~ FieldsDefinition.? ~> {
      (descr, comment, location, name, interfaces, dirs, fields) =>
        ast.ObjectTypeDefinition(
          name,
          interfaces,
          fields.fold(Vector.empty[ast.FieldDefinition])(_._1.toVector),
          dirs,
          descr,
          comment,
          fields.fold(Vector.empty[ast.Comment])(_._2),
          location
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
    (Comments ~ trackPos ~ extend ~ schema ~ (DirectivesConst.? ~> (_ getOrElse Vector.empty)) ~ wsNoComment(
      '{'
    ) ~ OperationTypeDefinition.+ ~ Comments ~ wsNoComment('}') ~> {
      (comment, location, dirs, ops, tc) =>
        ast.SchemaExtensionDefinition(ops.toVector, dirs, comment, tc, location)
    }) |
      (Comments ~ trackPos ~ extend ~ schema ~ DirectivesConst ~> {
        (comment, location, dirs) =>
          ast.SchemaExtensionDefinition(
            Vector.empty,
            dirs,
            comment,
            Vector.empty,
            location
          )
      })
  }

  def ObjectTypeExtensionDefinition = rule {
    (Comments ~ trackPos ~ extend ~ `type` ~ Name ~ (ImplementsInterfaces.? ~> (_ getOrElse Vector.empty)) ~ (DirectivesConst.? ~> (_ getOrElse Vector.empty)) ~ FieldsDefinition ~> {
      (comment, location, name, interfaces, dirs, fields) =>
        ast.ObjectTypeExtensionDefinition(
          name,
          interfaces,
          fields._1.toVector,
          dirs,
          comment,
          fields._2,
          location
        )
    }) |
      (Comments ~ trackPos ~ extend ~ `type` ~ Name ~ (ImplementsInterfaces.? ~> (_ getOrElse Vector.empty)) ~ DirectivesConst ~> {
        (comment, location, name, interfaces, dirs) =>
          ast.ObjectTypeExtensionDefinition(
            name,
            interfaces,
            Vector.empty,
            dirs,
            comment,
            Vector.empty,
            location
          )
      }) |
      (Comments ~ trackPos ~ extend ~ `type` ~ Name ~ ImplementsInterfaces ~> {
        (comment, location, name, interfaces) =>
          ast.ObjectTypeExtensionDefinition(
            name,
            interfaces,
            Vector.empty,
            Vector.empty,
            comment,
            Vector.empty,
            location
          )
      })
  }

  def InterfaceTypeExtensionDefinition = rule {
    (Comments ~ trackPos ~ extend ~ interface ~ Name ~ (DirectivesConst.? ~> (_ getOrElse Vector.empty)) ~ FieldsDefinition ~> {
      (comment, location, name, dirs, fields) =>
        ast.InterfaceTypeExtensionDefinition(
          name,
          fields._1.toVector,
          dirs,
          comment,
          fields._2,
          location
        )
    }) |
      (Comments ~ trackPos ~ extend ~ interface ~ Name ~ DirectivesConst ~> {
        (comment, location, name, dirs) =>
          ast.InterfaceTypeExtensionDefinition(
            name,
            Vector.empty,
            dirs,
            comment,
            Vector.empty,
            location
          )
      })
  }

  def UnionTypeExtensionDefinition = rule {
    (Comments ~ trackPos ~ extend ~ union ~ Name ~ (DirectivesConst.? ~> (_ getOrElse Vector.empty)) ~ UnionMemberTypes ~> {
      (comment, location, name, dirs, types) =>
        ast.UnionTypeExtensionDefinition(name, types, dirs, comment, location)
    }) |
      (Comments ~ trackPos ~ extend ~ union ~ Name ~ DirectivesConst ~> {
        (comment, location, name, dirs) =>
          ast.UnionTypeExtensionDefinition(
            name,
            Vector.empty,
            dirs,
            comment,
            location
          )
      })
  }

  def EnumTypeExtensionDefinition = rule {
    (Comments ~ trackPos ~ extend ~ enum ~ Name ~ (DirectivesConst.? ~> (_ getOrElse Vector.empty)) ~ EnumValuesDefinition ~> {
      (comment, location, name, dirs, values) =>
        ast.EnumTypeExtensionDefinition(
          name,
          values._1.toVector,
          dirs,
          comment,
          values._2,
          location
        )
    }) |
      (Comments ~ trackPos ~ extend ~ enum ~ Name ~ DirectivesConst ~> {
        (comment, location, name, dirs) =>
          ast.EnumTypeExtensionDefinition(
            name,
            Vector.empty,
            dirs,
            comment,
            Vector.empty,
            location
          )
      })
  }

  def InputObjectTypeExtensionDefinition = rule {
    (Comments ~ trackPos ~ extend ~ inputType ~ Name ~ (DirectivesConst.? ~> (_ getOrElse Vector.empty)) ~ InputFieldsDefinition ~> {
      (comment, location, name, dirs, fields) =>
        ast.InputObjectTypeExtensionDefinition(
          name,
          fields._1.toVector,
          dirs,
          comment,
          fields._2,
          location
        )
    }) |
      (Comments ~ trackPos ~ extend ~ inputType ~ Name ~ DirectivesConst ~> {
        (comment, location, name, dirs) =>
          ast.InputObjectTypeExtensionDefinition(
            name,
            Vector.empty,
            dirs,
            comment,
            Vector.empty,
            location
          )
      })
  }

  def ScalarTypeExtensionDefinition = rule {
    (Comments ~ trackPos ~ extend ~ scalar ~ Name ~ DirectivesConst ~> {
      (comment, location, name, dirs) =>
        ast.ScalarTypeExtensionDefinition(name, dirs, comment, location)
    })
  }

  def ImplementsInterfaces = rule {
    test(legacyImplementsInterface) ~ implements ~ NamedType.+ ~> (_.toVector) |
      implements ~ ws('&').? ~ NamedType.+(ws('&')) ~> (_.toVector)
  }

  def FieldsDefinition = rule {
    wsNoComment('{') ~ (test(legacyEmptyFields) ~ FieldDefinition.* | FieldDefinition.+) ~ Comments ~ wsNoComment(
      '}'
    ) ~> (_ → _)
  }
  def FieldDefinition = rule {
    Description ~ Comments ~ trackPos ~ Name ~ (ArgumentsDefinition.? ~> (_ getOrElse Vector.empty)) ~ ws(
      ':'
    ) ~ Type ~ (Directives.? ~> (_ getOrElse Vector.empty)) ~> {
      (descr, comment, location, name, args, fieldType, dirs) =>
        ast.FieldDefinition(
          name,
          fieldType,
          args,
          dirs,
          descr,
          comment,
          location
        )
    }
  }

  def ArgumentsDefinition = rule {
    wsNoComment('(') ~ InputValueDefinition.+ ~ wsNoComment(')') ~> (_.toVector)
  }

  def InputValueDefinition = rule {
    Description ~ Comments ~ trackPos ~ Name ~ ws(':') ~ Type ~ DefaultValue.? ~ (DirectivesConst.? ~> (_ getOrElse Vector.empty)) ~> {
      (descr, comment, location, name, valueType, default, dirs) =>
        ast.InputValueDefinition(
          name,
          valueType,
          default,
          dirs,
          descr,
          comment,
          location
        )
    }
  }

  def InterfaceTypeDefinition = rule {
    Description ~ Comments ~ trackPos ~ interface ~ Name ~ (DirectivesConst.? ~> (_ getOrElse Vector.empty)) ~ FieldsDefinition.? ~> {
      (descr, comment, location, name, dirs, fields) =>
        ast.InterfaceTypeDefinition(
          name,
          fields.fold(Vector.empty[ast.FieldDefinition])(_._1.toVector),
          dirs,
          descr,
          comment,
          fields.fold(Vector.empty[ast.Comment])(_._2),
          location
        )
    }
  }

  def UnionTypeDefinition = rule {
    Description ~ Comments ~ trackPos ~ union ~ Name ~ (DirectivesConst.? ~> (_ getOrElse Vector.empty)) ~ (UnionMemberTypes.? ~> (_ getOrElse Vector.empty)) ~> {
      (descr, comment, location, name, dirs, members) =>
        ast.UnionTypeDefinition(name, members, dirs, descr, comment, location)
    }
  }

  def UnionMemberTypes = rule { wsNoComment('=') ~ UnionMembers }

  def UnionMembers = rule { ws('|').? ~ NamedType.+(ws('|')) ~> (_.toVector) }

  def EnumTypeDefinition = rule {
    Description ~ Comments ~ trackPos ~ enum ~ Name ~ (DirectivesConst.? ~> (_ getOrElse Vector.empty)) ~ EnumValuesDefinition.? ~> {
      (descr, comment, location, name, dirs, values) =>
        ast.EnumTypeDefinition(
          name,
          values.fold(Vector.empty[ast.EnumValueDefinition])(_._1.toVector),
          dirs,
          descr,
          comment,
          values.fold(Vector.empty[ast.Comment])(_._2),
          location
        )
    }
  }

  def EnumValuesDefinition = rule {
    wsNoComment('{') ~ EnumValueDefinition.+ ~ Comments ~ wsNoComment('}') ~> (_ → _)
  }

  def EnumValueDefinition = rule {
    Description ~ Comments ~ trackPos ~ Name ~ (DirectivesConst.? ~> (_ getOrElse Vector.empty)) ~> {
      (descr, comments, location, name, dirs) =>
        ast.EnumValueDefinition(name, dirs, descr, comments, location)
    }
  }

  def InputObjectTypeDefinition = rule {
    Description ~ Comments ~ trackPos ~ inputType ~ Name ~ (DirectivesConst.? ~> (_ getOrElse Vector.empty)) ~ InputFieldsDefinition.? ~> {
      (descr, comment, location, name, dirs, fields) =>
        ast.InputObjectTypeDefinition(
          name,
          fields.fold(Vector.empty[ast.InputValueDefinition])(_._1.toVector),
          dirs,
          descr,
          comment,
          fields.fold(Vector.empty[ast.Comment])(_._2),
          location
        )
    }
  }

  def InputFieldsDefinition = rule {
    wsNoComment('{') ~ (test(legacyEmptyFields) ~ InputValueDefinition.* | InputValueDefinition.+) ~ Comments ~ wsNoComment(
      '}'
    ) ~> (_ → _)
  }

  def DirectiveDefinition = rule {
    Description ~ Comments ~ trackPos ~ directive ~ '@' ~ NameStrict ~ (ArgumentsDefinition.? ~> (_ getOrElse Vector.empty)) ~ on ~ DirectiveLocations ~> {
      (descr, comment, location, name, args, locations) =>
        ast.DirectiveDefinition(
          name,
          args,
          locations,
          descr,
          comment,
          location
        )
    }
  }

  def DirectiveLocations = rule {
    ws('|').? ~ DirectiveLocation.+(wsNoComment('|')) ~> (_.toVector)
  }

  def DirectiveLocation = rule {
    Comments ~ trackPos ~ DirectiveLocationName ~> {
      (comment, location, name) =>
        ast.DirectiveLocation(name, comment, location)
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
    Description ~ Comments ~ trackPos ~ schema ~ (DirectivesConst.? ~> (_ getOrElse Vector.empty)) ~ wsNoComment(
      '{'
    ) ~ OperationTypeDefinition.+ ~ Comments ~ wsNoComment('}') ~> {
      (descr, comment, location, dirs, ops, tc) =>
        ast.SchemaDefinition(ops.toVector, dirs, descr, comment, tc, location)
    }
  }

  def OperationTypeDefinition = rule {
    Comments ~ trackPos ~ OperationType ~ ws(':') ~ NamedType ~> {
      (comment, location, opType, tpe) =>
        ast.OperationTypeDefinition(opType, tpe, comment, location)
    }
  }

  def Description = rule { StringValue.? }

}
