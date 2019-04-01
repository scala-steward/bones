package com.bones.protobuf

import java.io.{ByteArrayOutputStream, IOException}
import java.time.ZonedDateTime
import java.util.UUID

import cats.Applicative
import cats.data.{NonEmptyList, Validated}
import com.bones.data.Error.ExtractionError
import com.bones.data.Value._
import com.google.protobuf.CodedOutputStream
import shapeless.{::, HList, HNil, Nat}
import cats.implicits._




object ProtobufSequentialOutputInterpreter {


  type Path = Vector[String]
  type FieldNumber = Int
  type LastFieldNumber = Int
  type ComputeSize = () => Int
  type Encode = CodedOutputStream => Either[NonEmptyList[IOException], CodedOutputStream]
  type EncodeToProto[A] = FieldNumber => A => (ComputeSize, Encode)
  type EncodeGroupToProto[H<:HList] = LastFieldNumber => H =>  (ComputeSize, Encode)

  def encodeToBytes[A](dc: BonesSchema[A]): A => Array[Byte] = dc match {
    case x: XMapData[_,_,A] => {
      val group = kvpGroup(x.from).apply(0)
      (a: A) => {
        val hlist = x.fba(a)
        val (_, fEncode) = group(hlist)
        val os = new ByteArrayOutputStream()
        val cos: CodedOutputStream = CodedOutputStream.newInstance(os)
        fEncode(cos)
        cos.flush()
        os.flush()
        os.close()
        os.toByteArray
      }
    }
  }

  protected def kvpGroup[H<:HList,HL<:Nat](group: KvpGroup[H,HL]): EncodeGroupToProto[H] = {
    group match {
      case KvpNil => (fieldNumber: FieldNumber) => (_:HNil) => (() => 0, (os: CodedOutputStream) => Right(os))
      case op: KvpSingleValueHead[h, t, tl, o] => (fieldNumber: FieldNumber) =>
        val headF = valueDefinition(op.fieldDefinition.op)(fieldNumber + 1)
        val tailF = kvpGroup(op.tail)(fieldNumber + 1)
        (input: o) => {
          val headResult = headF(input.head)
          val tailResult = tailF(input.tail)
          val fCompute: ComputeSize = () => headResult._1() + tailResult._1()
          val fEncode: Encode = (outputStream: CodedOutputStream) => {
            Applicative[({type AL[AA] = Either[NonEmptyList[IOException], AA]})#AL]
              .map2(headResult._2(outputStream), tailResult._2(outputStream))((l1: CodedOutputStream, l2: CodedOutputStream) => {
                l2
              })
          }
          (fCompute, fEncode)
        }
      case op: KvpGroupHead[a, al, h, hl, t, tl] => (fieldNumber: FieldNumber) =>
        val headF = kvpGroup(op.head)(fieldNumber)
        val tailF = kvpGroup(op.tail)(fieldNumber)
        (input: H) => {
          val cast = input.asInstanceOf[h :: t]
          val headResult = headF(cast.head)
          val tailResult = tailF(cast.tail)
          val fCompute: ComputeSize = () => headResult._1() + tailResult._1()
          val fEncode = (outputStream: CodedOutputStream) => {
            Applicative[({type AL[AA] = Either[NonEmptyList[IOException], AA]})#AL]
              .map2(headResult._2(outputStream), tailResult._2(outputStream))((l1: CodedOutputStream, l2: CodedOutputStream) => {
                l2
              })
          }
          (fCompute, fEncode)
        }

      case op: OptionalKvpGroup[h,hl] => (fieldNumber: FieldNumber) =>
        val kvpGroupF = kvpGroup(op.kvpGroup)(fieldNumber)
        (input: Option[h] :: HNil) => {
//          val cast = input.asInstanceOf[Option[h] :: HNil]
          input.head match {
            case None => (
              () => 0,
              (outputStream: CodedOutputStream) => Right(outputStream)
            )
            case Some(h) => kvpGroupF(h)
          }
        }
    }
  }


  def valueDefinition[A](fgo: ValueDefinitionOp[A]): EncodeToProto[A] = {
    val result = fgo match {
      case op: OptionalValueDefinition[a] => (fieldNumber: FieldNumber) =>
        val fa = valueDefinition(op.valueDefinitionOp)(fieldNumber)
        (opt: Option[a]) =>
          val optB = opt.map(fa)
          (
            () => optB.fold(0)(_._1()),
            (outputStream: CodedOutputStream) => optB.fold[Either[NonEmptyList[IOException], CodedOutputStream]](Right(outputStream))(item => item._2(outputStream))
          )
      case ob: BooleanData => (fieldNumber: FieldNumber) =>
        (bool: Boolean) => (
          () => CodedOutputStream.computeBoolSize(fieldNumber, bool),
          write(_.writeBool(fieldNumber, bool))
        )
      case rs: StringData => (fieldNumber: FieldNumber) =>
        (str: String) => (
          () => CodedOutputStream.computeStringSize(fieldNumber, str),
          write(_.writeString(fieldNumber, str))
        )
      case ri: LongData => (fieldNumber: FieldNumber) =>
        (l: Long) => (
          () => CodedOutputStream.computeInt64Size(fieldNumber, l),
          write(_.writeInt64(fieldNumber, l))
        )
      case uu: UuidData => (fieldNumber: FieldNumber) =>
        (u: UUID) =>  (
          CodedOutputStream.computeStringSize(fieldNumber, u.toString),
          write(_.writeString(fieldNumber, u.toString))
        )
      case dd: DateTimeData => (fieldNumber: FieldNumber) =>
        (d: ZonedDateTime) => (
          CodedOutputStream.computeStringSize(fieldNumber, dd.dateFormat.format(d)),
          write(_.writeString(fieldNumber, dd.dateFormat.format(d)))
        )

      case bd: BigDecimalData => (fieldNumber: FieldNumber) =>
        (d: BigDecimal) =>
          (
            () => CodedOutputStream.computeStringSize(fieldNumber, d.toString()),
            write(_.writeString(fieldNumber, d.toString))
          )
      case ld: ListData[t] => (fieldNumber: FieldNumber) =>
        val ft = valueDefinition(ld.tDefinition)(fieldNumber)
        (l: List[t]) => {
          //          val cast = l.asInstanceOf[List[t]]
          val result = l.map(item => ft(item))
          (
            () => result.map(_._1.apply()).sum, //todo is this really a sum?
            (outputStream: CodedOutputStream) => result.map(_._2(outputStream))
          )
        }
      case ed: EitherData[a, b] => ??? // use oneOf
      case esd: EnumerationStringData[a] => (fieldNumber: FieldNumber) =>
        (a: A) => (
          () => CodedOutputStream.computeStringSize(fieldNumber, a.toString),
          write(_.writeString(fieldNumber, a.toString))
        )
      case esd: EnumStringData[a] => (fieldNumber: FieldNumber) =>
        (a: a) => (
          () => CodedOutputStream.computeStringSize(fieldNumber, a.toString),
          write(_.writeString(fieldNumber, a.toString))
        )
      case kvp: KvpGroupData[h, hl] => (fieldNumber: FieldNumber) =>
        val enc = kvpGroup(kvp.kvpGroup)(0)
        (h: h) => enc(h)
      case x: XMapData[h,hl,a] => (fieldNumber: FieldNumber) =>
        val group = kvpGroup(x.from).apply(0)
        (a: A) => {
          val hlist = x.fba(a)
          val (fSize, fEncode) = group(hlist)
          val encodeF: Encode = (outputStream: CodedOutputStream) => {
            outputStream.writeTag(fieldNumber, 2)
            outputStream.writeUInt32NoTag(fSize())
            fEncode(outputStream)
          }
          (fSize, encodeF)
        }

//        val dc = kvpGroup(kvp.from)(0)
//        (a: A) => {
//          val hlist = kvp.fba(a)
//          val (childSizeF, childEncodeF) = dc(hlist)
//          val size = childSizeF()
//          val sizeF: ComputeSize = () => size
//          val encodeF: Encode = (outputStream: CodedOutputStream) => {
//            outputStream.writeTag(fieldNumber, 2)
//            outputStream.writeUInt32NoTag(size)
//            childEncodeF(outputStream)
//          }
//          (sizeF, encodeF)
//        }

    }
    result.asInstanceOf[EncodeToProto[A]]
  }


  private def write(f: CodedOutputStream => Unit): Encode = (codedOutputStream: CodedOutputStream) =>
    try {
      f(codedOutputStream)
      Right(codedOutputStream)
    } catch {
      case ex: IOException => Left(NonEmptyList.one(ex))
    }

}