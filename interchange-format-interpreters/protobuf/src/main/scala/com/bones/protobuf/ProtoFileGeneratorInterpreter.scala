package com.bones.protobuf

import com.bones.data.{KvpCoNil, KvpCoproduct, KvpSingleValueLeft}
import com.bones.data._
import com.bones.data.custom.CNilF
import com.bones.syntax.NoAlgebra
import shapeless.{:+:, Coproduct, HList, Inl, Inr, Nat}

/**
  * Create a Protobuf file descriptor based on the Kvp.
  */
object ProtoFileGeneratorInterpreter {

  type Required = Boolean
  type Repeated = Boolean
  type Name = String
  type Index = Int

  object CustomInterpreter {
    /** using kind projector allows us to create a new interpreter by merging two existing interpreters.
      * see https://stackoverflow.com/a/60561575/387094
      * */
    def merge[L[_], R[_] <: Coproduct, A](li: CustomInterpreter[L],
                                          ri: CustomInterpreter[R]
                                         ): CustomInterpreter[Lambda[A => L[A] :+: R[A]]] =
      new CustomInterpreter[Lambda[A => L[A] :+: R[A]]] {
        override def toMessageField[A](lr: L[A] :+: R[A]): (Name, Index) => (MessageField, Vector[NestedType], Index) = lr match {
          case Inl(l) => li.toMessageField(l)
          case Inr(r) => ri.toMessageField(r)
        }
      }

    implicit class InterpreterOps[ALG[_]](val base: CustomInterpreter[ALG]) extends AnyVal {
      def ++[R[_] <: Coproduct](r: CustomInterpreter[R]): CustomInterpreter[Lambda[A => ALG[A] :+: R[A]]] =
        merge(base, r)
    }

    object CNilProtoFileCustomInterpreterEncoder extends CustomInterpreter[CNilF] {
      override def toMessageField[A](alg: CNilF[A]): (Name, Index) => (MessageField, Vector[NestedType], Index) = sys.error("unreachable code")
    }
  }

  trait CustomInterpreter[ALG[_]] {
    def toMessageField[A](alg: ALG[A]): (Name, Int) => (MessageField, Vector[NestedType], Int)
  }

  object NoAlgebraCustomInterpreter extends CustomInterpreter[NoAlgebra] {
    def toMessageField[A](alg: NoAlgebra[A]) = sys.error("Unreachable code")
  }

  /** Types that can be in a Message */
  sealed trait DataType {
    def name: String
  }
  case object Int32 extends DataType {
    val name = "int32"
  }
  case object Bool extends DataType {
    val name = "bool"
  }
  case object StringRequireUtf8 extends DataType {
    val name = "string"
  }
  case object Int64 extends DataType {
    val name = "int64"
  }
  case object FloatType extends DataType {
    val name = "float"
  }
  case object DoubleType extends DataType {
    val name = "double"
  }
  case object Bytes extends DataType {
    val name = "bytes"
  }
  case class NestedDataType(messageName: String) extends DataType {
    val name = messageName.capitalize
  }
  case class EitherDataType(name: String, l: MessageField, r: MessageField) extends DataType

  case class OneOf(messageName: String, fields: List[MessageField]) extends DataType {
    val name = messageName.capitalize
  }

  case class MessageField(dataType: DataType,
                          required: Boolean,
                          repeated: Boolean,
                          name: String,
                          index: Int)

  /** Definitions which can be embedded in the Message */
  trait NestedType {
    def name: String
  }
  case class NestedMessage(name: String, dataTypes: Vector[MessageField])
      extends NestedType

  case class Message
  (
    name: String,
    messageFields: Vector[MessageField],
    nestedTypes: Vector[NestedType]
  ) extends NestedType

  def messageFieldsToProtoFile
  (
    fields: Vector[ProtoFileGeneratorInterpreter.MessageField],
    indent: String,
    allowRequired: Boolean
  ): String = {
    fields
      .sortBy(_.index)
      .map(field => {
        field.dataType match {
          case EitherDataType(name, left, right) =>
            s"""
               | ${indent}oneof ${name} {
               |   ${indent}  ${left.dataType.name} ${left.name} = ${left.index};
               |   ${indent}  ${right.dataType.name} ${right.name} = ${right.index};
               | ${indent}}
               | """.stripMargin('|')
          case OneOf(name, messages) =>
            val messageString = messages.map(m => s"   ${indent}  ${m.dataType.name} ${toSnake(m.name)} = ${m.index};\n               |").mkString
            s"""
               |${indent}oneof ${name} {
               |${messageString}
               |${indent}}
             """.stripMargin
          case _ => {
            val repeatedRequired =
              if (field.repeated) "repeated"
              else if (!allowRequired) ""
              else if (field.required) "required"
              else "optional"
            s"${indent}${repeatedRequired} ${field.dataType.name} ${field.name} = ${field.index};"
          }
        }
      })
      .mkString("\n")
  }

  def nestedTypeToProtoFile(
      types: Vector[ProtoFileGeneratorInterpreter.NestedType]): String = {
    types.map {
      case n: NestedMessage => nestedMessageToProtoFile(n)
    } mkString ("\n")
  }

  def nestedMessageToProtoFile(message: NestedMessage): String = {
    s"""
       |  message ${message.name.capitalize} {
       |${messageFieldsToProtoFile(message.dataTypes, "    ", true)}
       |  }
       """.stripMargin

  }

  def fromSchemaToProtoFile[ALG[_], A](dc: BonesSchema[ALG, A], customInterpreter: CustomInterpreter[ALG]): String =
    messageToProtoFile(fromSchemaCustomAlgebra(dc, customInterpreter))

  def messageToProtoFile(message: Message): String = {
    s"""
       |message ${message.name} {
       |${messageFieldsToProtoFile(message.messageFields, "  ", true)}
       |
       |${nestedTypeToProtoFile(message.nestedTypes)}
       |}
     """.stripMargin
  }

  def fromSchema[A](dc: BonesSchema[NoAlgebra, A]): Message =
    fromSchemaCustomAlgebra(dc, NoAlgebraCustomInterpreter)

  def fromSchemaCustomAlgebra[ALG[_],A](dc: BonesSchema[ALG, A], customerInterpreter: CustomInterpreter[ALG]): Message = {
    dc match {
      case t: HListConvert[ALG, a, al, b] =>
        val (messageFields, nestedTypes, lastIndex) = kvpHList(t.from, customerInterpreter)(0)
        Message(t.manifestOfA.runtimeClass.getSimpleName,
                messageFields,
                nestedTypes)
    }
  }

  def kvpCoproduct[ALG[_], C<:Coproduct](co: KvpCoproduct[ALG, C], customerInterpreter: CustomInterpreter[ALG]):
    Int => (Vector[MessageField], Vector[NestedType], Int) = lastIndex => {
    co match{
      case nil: KvpCoNil[_] => (Vector.empty, Vector.empty, lastIndex)
      case op: KvpSingleValueLeft[ALG,l,r] @unchecked => {
        val left = determineValueDefinition(op.kvpValue, customerInterpreter)(op.manifestL.runtimeClass.getSimpleName, lastIndex)
        val right = kvpCoproduct(op.kvpTail, customerInterpreter)(left._3)
        (left._1 +: right._1, left._2 ++ right._2, right._3)
      }
    }
  }

  def kvpHList[ALG[_], H <: HList, HL <: Nat](group: KvpHList[ALG, H, HL], customerInterpreter: CustomInterpreter[ALG])
    : Int => (Vector[MessageField], Vector[NestedType], Int) = lastIndex => {
    group match {
      case nil: KvpNil[_] => (Vector.empty, Vector.empty, lastIndex)
      case op: KvpSingleValueHead[ALG, h, t, tl, a] @unchecked => {
        val thisIndex = lastIndex + 1
        val r = determineValueDefinition(op.fieldDefinition.dataDefinition, customerInterpreter)(op.fieldDefinition.key,
                                                       thisIndex)
        val (messageFields, nestedTypes, lastUsedIndex) =
          kvpHList(op.tail, customerInterpreter)(r._3)
        (messageFields :+ r._1, r._2 ++ nestedTypes, lastUsedIndex)
      }
      case op: KvpConcreteTypeHead[ALG, a, ht, nt] @unchecked =>
        val head = fromBonesSchema(op.bonesSchema, customerInterpreter)(lastIndex)
        val tail = kvpHList(op.tail, customerInterpreter)(head._3)
        (head._1 ++ tail._1, head._2 ++ tail._2, tail._3)
      case op: KvpHListHead[ALG, a, al, h, hl, t, tl] @unchecked =>
        val head = kvpHList(op.head, customerInterpreter)(lastIndex)
        val tail = kvpHList(op.tail, customerInterpreter)(head._3)
        (head._1 ++ tail._1, head._2 ++ tail._2, tail._3)
    }
  }

  def fromBonesSchema[ALG[_], A]
    (
      bonesSchema: BonesSchema[ALG,A],
      customerInterpreter: CustomInterpreter[ALG]
    ) : Int => (Vector[MessageField], Vector[NestedType], Int) = {

    bonesSchema match {
      case co: KvpCoproductConvert[ALG, c, a] @unchecked =>
        kvpCoproduct(co.from, customerInterpreter)
      case hl: HListConvert[ALG, h, n, a] @unchecked =>
        kvpHList(hl.from, customerInterpreter)
    }

  }

  def determineValueDefinition[ALG[_], A]
    (
      value: Either[KvpValue[A], ALG[A]],
      customerInterpreter: CustomInterpreter[ALG]
    ): (Name, Int) => (MessageField, Vector[NestedType], Int) =
    value match {
      case Left(kvp) => valueDefinition(kvp, customerInterpreter)
      case Right(alg) => customerInterpreter.toMessageField(alg)
    }

  def booleanMessageField(name: String, index: Int): (MessageField, Vector[NestedType], Int) =
    (MessageField(Bool, true, false, name, index), Vector.empty, index)

  def intMessageField(name: String, index: Int): (MessageField, Vector[NestedType], Int) =
    (MessageField(Int32, true, false, name, index), Vector.empty, index)

  def longMessageField(name: String, index: Int): (MessageField, Vector[NestedType], Int) =
    (MessageField(Int64, true, false, name, index), Vector.empty, index)

  def stringMessageField(name: String, index: Int): (MessageField, Vector[NestedType], Int) =
    (MessageField(StringRequireUtf8, true, false, name, index), Vector.empty, index)

  def byteArrayMessageField(name: String, index:Int): (MessageField, Vector[NestedType], Int) =
    (MessageField(Bytes, true, false, name, index), Vector.empty, index)

  def floatMessageField(name: String, index: Int): (MessageField, Vector[NestedType], Int) =
    (MessageField(FloatType, true, false, name, index), Vector.empty, index)

  def doubleMessageField(name: String, index: Int): (MessageField, Vector[NestedType], Int) =
    (MessageField(DoubleType, true, false, name, index), Vector.empty, index)

  def timestampMessageField(name: String, index: Int): (MessageField, Vector[NestedType], Int) = {
    val messageFields: Vector[NestedMessage] =
      Vector(
        NestedMessage("Timestamp",
          Vector(
            MessageField(Int64, true, false, "seconds", 1),
            MessageField(Int64, true, false, "nanos", 2)
          )
        )
      )
    (MessageField(NestedDataType("Timestamp"), true, false, name, index), messageFields, index)
  }



  def valueDefinition[ALG[_], A](fgo: KvpValue[A], customerInterpreter: CustomInterpreter[ALG])
    : (Name, Int) => (MessageField, Vector[NestedType], Int) =
    (name, index) =>
      fgo match {
        case op: OptionalKvpValueDefinition[ALG, a] @unchecked =>
          val result = determineValueDefinition(op.valueDefinitionOp, customerInterpreter)(name, index)
          (result._1.copy(required = false), result._2, index)
        case ob: BooleanData => booleanMessageField(name, index)
        case rs: StringData => stringMessageField(name, index)
        case df: ShortData => intMessageField(name, index)
        case id: IntData => intMessageField(name, index)
        case ri: LongData => longMessageField(name, index)
        case uu: UuidData => stringMessageField(name, index)
        case dd: LocalDateTimeData => timestampMessageField(name, index)
        case dt: LocalDateData => longMessageField(name, index)
        case lt: LocalTimeData => longMessageField(name, index)
        case fd: FloatData => floatMessageField(name, index)
        case fd: DoubleData => doubleMessageField(name, index)
        case bd: BigDecimalData => stringMessageField(name, index)
        case ba: ByteArrayData => byteArrayMessageField(name, index)
        case ld: ListData[ALG, t] @unchecked =>
          val result = determineValueDefinition(ld.tDefinition, customerInterpreter)(name, index)
          (result._1.copy(repeated = true), result._2, index)
        case ed: EitherData[ALG, a, b] @unchecked =>
          val (messageFieldA, nestedTypesA, nextIndex) = determineValueDefinition(ed.definitionA, customerInterpreter)(s"${name}Left", index)
          val (messageFieldB, nestedTypesB, lastIndex) = determineValueDefinition(ed.definitionB, customerInterpreter)(s"${name}Right", nextIndex + 1)
          val oneOfName = toSnake(name.capitalize)
          (MessageField(EitherDataType(oneOfName, messageFieldA, messageFieldB), false, false, name, index), Vector.empty, lastIndex)
        case esd: EnumerationData[e,a] => stringMessageField(name, index)
        case kvp: KvpCoproductValue[ALG, c] @unchecked =>
          val (fields, nestedTypes,nextIndex) = kvpCoproduct(kvp.kvpCoproduct, customerInterpreter)(index)
          val nestedMessageFields: Vector[MessageField] = nestedTypes.zipWithIndex.map(nt => MessageField(NestedDataType(nt._1.name), false, false, nt._1.name, index + nt._2))
          val name = nestedTypes.headOption.map(_.name).getOrElse("unknown")
          (MessageField(OneOf(name + "_oneof", nestedMessageFields.toList), true, false, name, nextIndex), nestedTypes, index + nestedMessageFields.length - 1)
        case kvp: KvpHListValue[ALG, h, hl] @unchecked =>
          val result = kvpHList(kvp.kvpHList, customerInterpreter)(0)
          val nested = NestedMessage(name, result._1)
          (MessageField(NestedDataType(name), true, false, name, index),
           Vector(nested), index)
        case t: HListConvert[ALG, h, hl, a] @unchecked =>
          val (messageFields, _, _) = kvpHList(t.from, customerInterpreter)(0)
          val nested = NestedMessage(name, messageFields)
          (MessageField(NestedDataType(name), true, false, name, index),
           Vector(nested), index)
        case co: KvpCoproductConvert[ALG, c,a] @unchecked =>
          val (fields, nestedTypes, nextIndex) = kvpCoproduct(co.from, customerInterpreter)(index)
          val nestedMessageFields: Vector[MessageField] = nestedTypes.zipWithIndex.map(nt => MessageField(NestedDataType(nt._1.name), false, false, nt._1.name, index + nt._2))
          val name = nestedTypes.headOption.map(_.name).getOrElse("unknown")
          (MessageField(OneOf(name + "_oneof", nestedMessageFields.toList), true, false, name, nextIndex), nestedTypes, index + nestedMessageFields.length - 1)


    }

  def toSnake(str: String) = {
    str.replaceAll("([A-Z]+)([A-Z][a-z])", "$1_$2").replaceAll("([a-z\\d])([A-Z])", "$1_$2").toLowerCase
  }

}
