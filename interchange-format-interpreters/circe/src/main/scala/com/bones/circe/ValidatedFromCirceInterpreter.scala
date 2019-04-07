package com.bones.circe

import java.nio.charset.Charset
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID

import cats.data.NonEmptyList
import com.bones.data.Error.{ExtractionError, ParsingError, RequiredData, WrongTypeError}
import com.bones.data.KeyValueDefinition
import com.bones.data.Value._
import com.bones.interpreter.KvpValidateInputInterpreter
import com.bones.Util._
import io.circe.Json

object ValidatedFromCirceInterpreter extends KvpValidateInputInterpreter[Json] {


  def fromByteArray(arr: Array[Byte], charSet: Charset): Either[NonEmptyList[ParsingError[Array[Byte]]], Json] = {
    val input = new String(arr, charSet)
    io.circe.parser.parse(input).left.map(x => NonEmptyList.one(ParsingError(x.message)))
  }

  protected def invalidValue[T](json: Json, expected: Class[T], path: List[String]):
  Left[NonEmptyList[WrongTypeError[T]], Nothing] = {
    val invalid = json.fold(
      classOf[Nothing],
      _ => classOf[Boolean],
      _ => classOf[Number],
      _ => classOf[String],
      _ => classOf[Array[_]],
      _ => classOf[Object]
    )
    Left(NonEmptyList.one(WrongTypeError(path, expected, invalid)))
  }

  protected def determineError[A](in: Json, op: ValueDefinitionOp[A], expectedType: Class[_], path: List[String]): NonEmptyList[ExtractionError] = {
    val error =
      if (in.isNull) RequiredData(path, op)
      else  WrongTypeError(path, expectedType, in.getClass)
    NonEmptyList.one(error)
  }


  override def headValue[A](in: Json,
                            kv: KeyValueDefinition[A],
                            headInterpreter: (Option[Json], List[String]) => Either[NonEmptyList[ExtractionError], A],
                            path: List[String]): Either[NonEmptyList[ExtractionError], A] =
    in.asObject match {
      case Some(jsonObj) =>
        val fields = jsonObj.toList
        headInterpreter(fields.find(_._1 == kv.key).map(_._2), path)
      case None => Left(NonEmptyList.one(WrongTypeError(path, classOf[Object], in.getClass)))
    }


  override def extractString[A](op: ValueDefinitionOp[A], clazz: Class[_])(in: Json, path: List[String]):
    Either[NonEmptyList[ExtractionError], String] =
      in.asString.toRight(determineError(in,op,clazz, path))


  override def extractLong(op: LongData)(in: Json, path: List[String]):
    Either[NonEmptyList[WrongTypeError[Long]], Long] =
    in.asNumber.flatMap(_.toLong).toRight(NonEmptyList.one(WrongTypeError(path,classOf[Long], in.getClass)))


  override def extractBool(op: BooleanData)(in: Json, path: List[String]): Either[NonEmptyList[ExtractionError], Boolean] =
    in.asBoolean.toRight(determineError(in, op, classOf[Boolean], path))


  override def extractUuid(op: UuidData)(in: Json, path: List[String]): Either[NonEmptyList[ExtractionError], UUID] =
    in.asString
      .toRight(determineError(in, op, classOf[UUID], path))
      .flatMap(stringToUuid(_,path))


  override def extractZonedDateTime(dateFormat: DateTimeFormatter, op: DateTimeData)(in: Json, path: List[String]):
    Either[NonEmptyList[ExtractionError], ZonedDateTime] =
    in.asString
      .toRight(determineError(in, op, classOf[ZonedDateTime], path))
      .flatMap(stringToZonedDateTime(_,dateFormat, path))


  override def extractArray[A](op: ListData[A])(in: Json, path: List[String]): Either[NonEmptyList[ExtractionError], Seq[Json]] =
    in.asArray
      .toRight(determineError(in, op, classOf[List[A]], path))


  override def extractBigDecimal(op: BigDecimalData)(in: Json, path: List[String]):
  Either[NonEmptyList[ExtractionError], BigDecimal] =
    in.asNumber
      .flatMap(_.toBigDecimal)
      .toRight(determineError(in, op, classOf[BigDecimal], path))


}
