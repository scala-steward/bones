package com.bones.interpreter.custom

import java.time._
import java.time.format.{DateTimeFormatter, DateTimeParseException}

import cats.data.NonEmptyList
import com.bones.data.Error
import com.bones.data.Error.{CanNotConvert, ExtractionError, RequiredValue}
import com.bones.data.algebra._
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.InterchangeFormatValidator
import com.bones.validation.algebra.ScalaCoreValidation.ValidationOp
import com.bones.validation.ValidationUtil

object JavaTimeValidator {

  def errorHandleTimeParsing[A](
    path: List[String],
    f: String => A,
    input: String): Either[NonEmptyList[Error.ExtractionError], A] =
    try {
      Right(f(input))
    } catch {
      case ex: DateTimeParseException =>
        Left(NonEmptyList.one(CanNotConvert(path, input, classOf[LocalDateTime], Some(ex))))
      case ex: IllegalArgumentException =>
        Left(NonEmptyList.one(CanNotConvert(path, input, classOf[LocalDateTime], Some(ex))))
    }

  def parseTime[A, OUT](
    baseValidator: KvpInterchangeFormatValidatorInterpreter[OUT],
    alg: JavaTimeValue[A],
    clazz: Class[A],
    f: String => A,
    validations: List[ValidationOp[A]]
  ): (Option[OUT], List[String]) => Either[NonEmptyList[Error.ExtractionError], A] = {
    (jsonOpt: Option[OUT], path: List[String]) =>
      {
        jsonOpt match {
          case Some(json) =>
            baseValidator
              .extractString(Right(alg), clazz)(json, path)
              .flatMap(result => errorHandleTimeParsing(path, f, result))
              .flatMap(result => ValidationUtil.validate(validations)(result, path))
          case None =>
            Left(NonEmptyList.one(RequiredValue(path, Right(alg))))
        }
      }
  }

  def parseYear[A, OUT](
    baseValidator: KvpInterchangeFormatValidatorInterpreter[OUT],
    alg: JavaTimeValue[A],
    validations: List[ValidationOp[Year]])
    : (Option[OUT], List[String]) => Either[NonEmptyList[Error.ExtractionError], Year] =
    (jsonOpt: Option[OUT], path: List[String]) => {
      jsonOpt match {
        case Some(json) =>
          baseValidator
            .extractInt(Right(alg))(json, path)
            .map(result => Year.of(result))
            .flatMap(result => ValidationUtil.validate(validations)(result, path))
        case None =>
          Left(NonEmptyList.one(RequiredValue(path, Right(alg))))
      }
    }
}

trait JavaTimeValidator[OUT] extends InterchangeFormatValidator[JavaTimeValue, OUT] {

  import JavaTimeValidator._

  val instantFormatter: DateTimeFormatter
  val offsetDateTimeFormatter: DateTimeFormatter
  val offsetTimeFormatter: DateTimeFormatter
  val zonedDateTimeFormatter: DateTimeFormatter
  val baseValidator: KvpInterchangeFormatValidatorInterpreter[OUT]

  def localDateTimeFormatter: DateTimeFormatter
  def localDateFormatter: DateTimeFormatter
  def localTimeFormatter: DateTimeFormatter

  override def extractLocalDateTime[ALG[_], A](dataDefinition: ALG[A])(
    in: OUT,
    path: List[String]): Either[NonEmptyList[ExtractionError], LocalDateTime] = {
    extractString(dataDefinition, classOf[LocalDateTime])(in, path)
      .flatMap(stringToLocalDateTime(_, localDateTimeFormatter, path))
  }

  override def extractLocalDate[ALG[_], A](dataDefinition: ALG[A])(
    in: OUT,
    path: List[String]): Either[NonEmptyList[ExtractionError], LocalDate] = {
    extractString(dataDefinition, classOf[LocalDate])(in, path)
      .flatMap(stringToLocalDate(_, localDateFormatter, path))
  }

  override def extractLocalTime[ALG[_], A](dataDefinition: ALG[A], baseInterpreter: KvpInterchangeFormatValidatorInterpreter[OUT])(
    in: OUT,
    path: List[String]): Either[NonEmptyList[ExtractionError], LocalTime] = {
    baseInterpreter.extractString(dataDefinition, classOf[LocalTime])(in, path)
      .flatMap(stringToLocalTime(_, localTimeFormatter, path))
  }


  override def validate[A](
                            alg: JavaTimeValue[A],
                            baseInterpreter: KvpInterchangeFormatValidatorInterpreter[OUT]
                          ): (Option[OUT], List[String]) => Either[NonEmptyList[ExtractionError], A] =

    alg match {
      case d: DateTimeExceptionData =>
        parseTime(
          baseValidator,
          alg,
          classOf[DateTimeException],
          input => new DateTimeException(input),
          d.validations)
      case d: DayOfWeekData =>
        parseTime(baseValidator, alg, classOf[DayOfWeek], DayOfWeek.valueOf, d.validations)
      case d: DurationData =>
        parseTime(baseValidator, alg, classOf[Duration], Duration.parse, d.validations)
      case i: InstantData =>
        parseTime(
          baseValidator,
          alg,
          classOf[Instant],
          input => Instant.from(instantFormatter.parse(input)),
          i.validations)
      case m: MonthData =>
        parseTime(baseValidator, alg, classOf[Month], Month.valueOf, m.validations)
      case m: MonthDayData =>
        parseTime(baseValidator, alg, classOf[MonthDay], MonthDay.parse, m.validations)
      case o: OffsetDateTimeData =>
        parseTime(
          baseValidator,
          alg,
          classOf[OffsetDateTime],
          OffsetDateTime.parse(_, offsetDateTimeFormatter),
          o.validations)
      case o: OffsetTimeData =>
        parseTime(
          baseValidator,
          alg,
          classOf[OffsetTime],
          OffsetTime.parse(_, offsetTimeFormatter),
          o.validations)
      case p: PeriodData =>
        parseTime(baseValidator, alg, classOf[Period], Period.parse, p.validations)
      case y: YearData => parseYear(baseValidator, alg, y.validations)
      case y: YearMonthData =>
        parseTime(baseValidator, alg, classOf[YearMonth], YearMonth.parse, y.validations)
      case z: ZoneIdData =>
        parseTime(baseValidator, alg, classOf[ZoneId], ZoneId.of, z.validations)
      case z: ZonedDateTimeData =>
        parseTime(
          baseValidator,
          alg,
          classOf[ZonedDateTime],
          ZonedDateTime.parse(_, zonedDateTimeFormatter),
          z.validations)
      case z: ZoneOffsetData =>
        parseTime(baseValidator, alg, classOf[ZoneOffset], ZoneOffset.of, z.validations)
    }
  }

}
