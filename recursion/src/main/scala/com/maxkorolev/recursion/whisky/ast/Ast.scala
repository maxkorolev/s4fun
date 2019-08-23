package com.maxkorolev.recursion.whisky.ast

import com.maxkorolev.recursion.whisky.parser.SourceMapper
import higherkindness.droste.data._

object Ast {

  def apply(
      head: AstLocation,
      tail: AstNode[Attr[AstNode, AstLocation]]
  ): Attr[AstNode, AstLocation] =
    Attr((head, tail))
}

sealed trait AstNode[+Z]

sealed trait SchemaAstNode[+Z] extends AstNode[Z]
sealed trait TypeSystemDefinition[+Z]
    extends SchemaAstNode[Z]
    with Definition[Z]
sealed trait TypeSystemExtensionDefinition[+Z]
    extends SchemaAstNode[Z]
    with Definition[Z]

sealed trait TypeDefinition[+Z] extends TypeSystemDefinition[Z] {
  def name: String
}

sealed trait TypeExtensionDefinition[+Z]
    extends TypeSystemExtensionDefinition[Z] {
  def name: String
}

sealed trait ObjectLikeTypeExtensionDefinition[+Z]
    extends TypeExtensionDefinition[Z] {
  def fields: List[Z]
}

case class Document[+Z](
    definitions: List[Z],
    trailingComments: List[Z] = Nil
) extends AstNode[Z]

case class InputDocument[+Z](
    values: List[Z],
    trailingComments: List[Z] = Nil
) extends AstNode[Z]

sealed trait Definition[+Z] extends AstNode[Z]

case class OperationDefinition[+Z](
    operationType: OperationType = OperationType.Query,
    name: Option[String] = None,
    variables: List[Z] = Nil,
    directives: List[Z] = Nil,
    selections: List[Z],
    comments: List[Z] = Nil,
    trailingComments: List[Z] = Nil
) extends Definition[Z]

case class FragmentDefinition[+Z](
    name: String,
    typeCondition: Z,
    directives: List[Z],
    selections: List[Z],
    variables: List[Z] = Nil,
    comments: List[Z] = Nil,
    trailingComments: List[Z] = Nil
) extends Definition[Z]

sealed trait OperationType

object OperationType {
  case object Query extends OperationType
  case object Mutation extends OperationType
  case object Subscription extends OperationType
}

case class VariableDefinition[+Z](
    name: String,
    tpe: Z,
    defaultValue: Option[Z],
    directives: List[Z] = Nil,
    comments: List[Z] = Nil
) extends AstNode[Z]

sealed trait Type[+Z] extends AstNode[Z]

case class NamedType[+Z](name: String) extends Type[Z]
case class NotNullType[+Z](ofType: Z) extends Type[Z]
case class ListType[+Z](ofType: Z) extends Type[Z]

sealed trait Selection[+Z] extends AstNode[Z]

case class Field[+Z](
    alias: Option[String],
    name: String,
    arguments: List[Z],
    directives: List[Z],
    selections: List[Z],
    comments: List[Z] = Nil,
    trailingComments: List[Z] = Nil
) extends Selection[Z]

case class FragmentSpread[+Z](
    name: String,
    directives: List[Z],
    comments: List[Z] = Nil
) extends Selection[Z]

case class InlineFragment[+Z](
    typeCondition: Option[Z],
    directives: List[Z],
    selections: List[Z],
    comments: List[Z] = Nil,
    trailingComments: List[Z] = Nil
) extends Selection[Z]

case class SelectionSet[+Z](
    selections: List[Z],
    comments: List[Z] = Nil
) extends AstNode[Z]

case class Directive[+Z](
    name: String,
    arguments: List[Z],
    comments: List[Z] = Nil
) extends AstNode[Z]

case class Argument[+Z](
    name: String,
    value: Z,
    comments: List[Z] = Nil
) extends AstNode[Z]

sealed trait Value[+Z] extends AstNode[Z]

sealed trait ScalarValue[+Z] extends Value[Z]

case class IntValue[+Z](value: Int, comments: List[Z] = Nil)
    extends ScalarValue[Z]

case class BigIntValue[+Z](
    value: BigInt,
    comments: List[Z] = Nil
) extends ScalarValue[Z]

case class FloatValue[+Z](
    value: Double,
    comments: List[Z] = Nil
) extends ScalarValue[Z]

case class BigDecimalValue[+Z](
    value: BigDecimal,
    comments: List[Z] = Nil
) extends ScalarValue[Z]

case class StringValue[+Z](
    value: String,
    block: Boolean = false,
    blockRawValue: Option[String] = None,
    comments: List[Z] = Nil
) extends ScalarValue[Z]

case class BooleanValue[+Z](
    value: Boolean,
    comments: List[Z] = Nil
) extends ScalarValue[Z]

case class EnumValue[+Z](
    value: String,
    comments: List[Z] = Nil
) extends Value[Z]

case class ListValue[+Z](
    values: List[Z],
    comments: List[Z] = Nil
) extends Value[Z]

case class VariableValue[+Z](
    name: String,
    comments: List[Z] = Nil
) extends Value[Z]

case class NullValue[+Z](
    comments: List[Z] = Nil
) extends Value[Z]

case class ObjectValue[+Z](
    fields: List[Z],
    comments: List[Z] = Nil
) extends Value[Z]

case class ObjectField[+Z](
    name: String,
    value: Z,
    comments: List[Z] = Nil
) extends AstNode[Z]

case class Comment[+Z](text: String) extends AstNode[Z]

// Schema Definition

case class ScalarTypeDefinition[+Z](
    name: String,
    directives: List[Z] = Nil,
    description: Option[Z] = None,
    comments: List[Z] = Nil
) extends TypeDefinition[Z]

case class FieldDefinition[+Z](
    name: String,
    fieldType: Z,
    arguments: List[Z],
    directives: List[Z] = Nil,
    description: Option[Z] = None,
    comments: List[Z] = Nil
) extends SchemaAstNode[Z]

case class InputValueDefinition[+Z](
    name: String,
    valueType: Z,
    defaultValue: Option[Z],
    directives: List[Z] = Nil,
    description: Option[Z] = None,
    comments: List[Z] = Nil
) extends SchemaAstNode[Z]

case class ObjectTypeDefinition[+Z](
    name: String,
    interfaces: List[Z],
    fields: List[Z],
    directives: List[Z] = Nil,
    description: Option[Z] = None,
    comments: List[Z] = Nil,
    trailingComments: List[Z] = Nil
) extends TypeDefinition[Z]

case class InterfaceTypeDefinition[+Z](
    name: String,
    fields: List[Z],
    directives: List[Z] = Nil,
    description: Option[Z] = None,
    comments: List[Z] = Nil,
    trailingComments: List[Z] = Nil
) extends TypeDefinition[Z]

case class UnionTypeDefinition[+Z](
    name: String,
    types: List[Z],
    directives: List[Z] = Nil,
    description: Option[Z] = None,
    comments: List[Z] = Nil
) extends TypeDefinition[Z]

case class EnumTypeDefinition[+Z](
    name: String,
    values: List[Z],
    directives: List[Z] = Nil,
    description: Option[Z] = None,
    comments: List[Z] = Nil,
    trailingComments: List[Z] = Nil
) extends TypeDefinition[Z]

case class EnumValueDefinition[+Z](
    name: String,
    directives: List[Z] = Nil,
    description: Option[Z] = None,
    comments: List[Z] = Nil
) extends SchemaAstNode[Z]

case class InputObjectTypeDefinition[+Z](
    name: String,
    fields: List[Z],
    directives: List[Z] = Nil,
    description: Option[Z] = None,
    comments: List[Z] = Nil,
    trailingComments: List[Z] = Nil
) extends TypeDefinition[Z]

case class ObjectTypeExtensionDefinition[+Z](
    name: String,
    interfaces: List[Z],
    fields: List[Z],
    directives: List[Z] = Nil,
    comments: List[Z] = Nil,
    trailingComments: List[Z] = Nil
) extends ObjectLikeTypeExtensionDefinition[Z]

case class InterfaceTypeExtensionDefinition[+Z](
    name: String,
    fields: List[Z],
    directives: List[Z] = Nil,
    comments: List[Z] = Nil,
    trailingComments: List[Z] = Nil
) extends ObjectLikeTypeExtensionDefinition[Z]

case class InputObjectTypeExtensionDefinition[+Z](
    name: String,
    fields: List[Z],
    directives: List[Z] = Nil,
    comments: List[Z] = Nil,
    trailingComments: List[Z] = Nil
) extends TypeExtensionDefinition[Z]

case class EnumTypeExtensionDefinition[+Z](
    name: String,
    values: List[Z],
    directives: List[Z] = Nil,
    comments: List[Z] = Nil,
    trailingComments: List[Z] = Nil
) extends TypeExtensionDefinition[Z]

case class UnionTypeExtensionDefinition[+Z](
    name: String,
    types: List[Z],
    directives: List[Z] = Nil,
    comments: List[Z] = Nil
) extends TypeExtensionDefinition[Z]

case class ScalarTypeExtensionDefinition[+Z](
    name: String,
    directives: List[Z] = Nil,
    comments: List[Z] = Nil
) extends TypeExtensionDefinition[Z]

case class SchemaExtensionDefinition[+Z](
    operationTypes: List[Z],
    directives: List[Z] = Nil,
    comments: List[Z] = Nil,
    trailingComments: List[Z] = Nil
) extends TypeSystemExtensionDefinition[Z]

case class DirectiveDefinition[+Z](
    name: String,
    arguments: List[Z],
    locations: List[Z],
    description: Option[Z] = None,
    comments: List[Z] = Nil
) extends TypeSystemDefinition[Z]

case class DirectiveLocation[+Z](
    name: String,
    comments: List[Z] = Nil
) extends SchemaAstNode[Z]

case class SchemaDefinition[+Z](
    operationTypes: List[Z],
    directives: List[Z] = Nil,
    description: Option[Z] = None,
    comments: List[Z] = Nil,
    trailingComments: List[Z] = Nil
) extends TypeSystemDefinition[Z]

case class OperationTypeDefinition[+Z](
    operation: OperationType,
    tpe: Z,
    comments: List[Z] = Nil
) extends SchemaAstNode[Z]

case class AstLocation(sourceId: String, index: Int, line: Int, column: Int)

object AstLocation {
  def apply(index: Int, line: Int, column: Int): AstLocation =
    AstLocation("", index, line, column)
}
