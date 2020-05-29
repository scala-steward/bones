package com.bones.interpreter

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime, LocalTime}
import java.util.{Base64, UUID}

import cats.data.NonEmptyList
import com.bones.Util
import com.bones.Util.{stringToLocalDate, stringToLocalDateTime, stringToLocalTime}
import com.bones.data.Error._
import com.bones.data.HasManifest.AlgebraWithManifest
import com.bones.data.algebra.{CNilF, HListConvert, KvpCoproductConvert, ListData}
import com.bones.data.{KeyValueDefinition, KvpCoNil, KvpCoproduct, KvpSingleValueLeft, _}
import com.bones.validation.algebra.ScalaCoreValidation.ValidationOp
import com.bones.validation.{ValidationUtil => vu}
import shapeless.{:+:, ::, Coproduct, HList, HNil, Inl, Inr, Nat}

import scala.util.Try


/**
 * This is our typeclass for validate.
 * @tparam A
 */
trait Validate[A] {
  type ALG[_]
  val algebra: ALG[_]


  def validateFromByteArray(arr: Array[Byte]): Either[ExtractionError, A]
}

object KvpInterchangeFormatValidatorInterpreter {

  type Path = List[String]

  /** Represents a path to an element, such as List("someClass", "someMember", "someField") */
  type CoproductType = String


  object InterchangeFormatValidator {

    /** using kind projector allows us to create a new interpreter by merging two existing interpreters */
    def merge[L[_], R[_] <: Coproduct, A, OUT](
      li: InterchangeFormatValidator[L, OUT],
      ri: InterchangeFormatValidator[R, OUT]
    ): InterchangeFormatValidator[Lambda[A => L[A] :+: R[A]], OUT] =
      new InterchangeFormatValidator[Lambda[A => L[A] :+: R[A]], OUT] {
        override def validate[A](lr: L[A] :+: R[A])
          : (Option[OUT], List[String]) => Either[NonEmptyList[ExtractionError], A] = lr match {
          case Inl(l) => li.validate(l)
          case Inr(r) => ri.validate(r)
        }
      }

    implicit class InterpreterOps[ALG[_], OUT](val base: InterchangeFormatValidator[ALG, OUT])
        extends AnyVal {
      def ++[R[_] <: Coproduct](r: InterchangeFormatValidator[R, OUT])
        : InterchangeFormatValidator[Lambda[A => ALG[A] :+: R[A]], OUT] =
        merge(base, r)
    }

    case class CNilInterchangeFormatValidator[OUT]()
        extends InterchangeFormatValidator[CNilF, OUT] {

      override def validate[A](alg: CNilF[A]): (Option[OUT], List[String]) => Either[NonEmptyList[ExtractionError], A] =
        sys.error("Unreachable code")
    }
  }

  trait InterchangeFormatValidator[ALG[_], IN] {
    def validate[A](alg: ALG[A], validator: InterchangeFormatValidator[ALG, IN]): (Option[IN], List[String]) => Either[NonEmptyList[ExtractionError], A]
  }

  /**
    * Validator which converts a sting to a date using the supplied date formats.  Useful
    * for JSON data types.  Is meant to be a mix-in for KvpInterchangeFormatValidatorInterpreter.
    * @tparam OUT
    */
  trait StringToDateValidator[OUT] { self: KvpInterchangeFormatValidatorInterpreter[OUT] =>

    def localDateTimeFormatter: DateTimeFormatter
    def localDateFormatter: DateTimeFormatter
    def localTimeFormatter: DateTimeFormatter

    override def extractLocalDateTime[ALG[_], A](dataDefinition: ALG[A])(
      in: OUT,
      path: Path): Either[NonEmptyList[ExtractionError], LocalDateTime] = {
      extractString(dataDefinition, classOf[LocalDateTime])(in, path)
        .flatMap(stringToLocalDateTime(_, localDateTimeFormatter, path))
    }

    override def extractLocalDate[ALG[_], A](dataDefinition: ALG[A])(
      in: OUT,
      path: Path): Either[NonEmptyList[ExtractionError], LocalDate] = {
      extractString(dataDefinition, classOf[LocalDate])(in, path)
        .flatMap(stringToLocalDate(_, localDateFormatter, path))
    }

    override def extractLocalTime[ALG[_], A](dataDefinition: ALG[A])(
      in: OUT,
      path: Path): Either[NonEmptyList[ExtractionError], LocalTime] = {
      extractString(dataDefinition, classOf[LocalTime])(in, path)
        .flatMap(stringToLocalTime(_, localTimeFormatter, path))
    }
  }

}

/**
  * Base trait for converting from an interchange format such as JSON to an HList or Case class.
  * @tparam IN The base data type of the interchange format, such as io.circe.Json
  */
trait KvpInterchangeFormatValidatorInterpreter[IN] { baseValidator =>

  /** An additional string in the serialized format which states the coproduct type */
  val coproductTypeKey: String

  import KvpInterchangeFormatValidatorInterpreter._
  import Util._

  /** Entry point for this class to convert a Schema into a function where
    * the function takes an interchange format, validates it and if successful, spits out
    * the resulting data..
    * @param schema The schema used to validate and decode this input data.
    * @tparam A The final data type returned by the resulting function.
    * @return A function which takes the IN data and returns an Either[ExtractionError,A]
    */
  def validatorFromSchema[ALG[_], A](
    schema: BonesSchema[ALG, A],
    interchangeFormatValidator: InterchangeFormatValidator[ALG, IN])
    : IN => Either[NonEmptyList[ExtractionError], A] =
      in => interchangeFormatValidator.validate(schema.alg, baseValidator).apply(Some(in), List.empty)


  def validatorFromSchemaWithPath[ALG[_], A](
    schema: BonesSchema[ALG, A],
    interchangeFormatValidator: InterchangeFormatValidator[ALG, IN])
    : (IN, Path) => Either[NonEmptyList[ExtractionError], A] =
    schema.alg match {
      case x: HListConvert[ALG, _, _, A] =>
        (in, path) => interchangeFormatValidator.validate(x, baseValidator)(Some(in), path)
      case co: KvpCoproductConvert[ALG, c, A] =>
        val coproductF = kvpCoproduct[ALG, c](co.from, interchangeFormatValidator)
        (in, path) =>
          {
            stringValue(in, coproductTypeKey) match {
              case Some(coType) => {
                val resultC = coproductF(in, path, coType)
                resultC.map(co.cToA(_))
              }
              case None =>
                Left(NonEmptyList.one(SumTypeError(path, s"Missing parameter ${coproductTypeKey}")))
            }
          }
    }

  /**
    * Extend this to extract the value of type A from the input type IN
    * @param in The input type, for instance a base Json type.
    * @param kv The key and value definition describing this extraction.
    * @param headInterpreterF we will pass the appropriate extractXXX type based on type A
    * @param path The json path to the element such
    * @tparam A The type being extracted.
    * @return Either successful A or failure.  Should probably just return result from headInterpreterF.
    */
  protected def headValue[ALG[_], A](
    in: IN,
    kv: KeyValueDefinition[ALG, A],
    headInterpreterF: (Option[IN], Path) => Either[NonEmptyList[ExtractionError], A],
    path: Path): Either[NonEmptyList[ExtractionError], A]

  /**
    * Override this to provide the ability to extract a String from the IN type.
    * @param clazz The resulting class we are tyring to extract.
    * @param in The interchange format input type.
    * @param path The path hierarchy.
    * @tparam A The expected resulting type, eg String or Enumerated Type which we are trying to extract from a string.
    * @return The extracted String or an Error
    */
  def extractString[ALG[_], A](dataDefinition: ALG[A], clazz: Class[_])(
    in: IN,
    path: Path): Either[NonEmptyList[ExtractionError], String]
  def extractInt[ALG[_], A](dataDefinition: ALG[A])(
    in: IN,
    path: Path): Either[NonEmptyList[ExtractionError], Int]
  def extractLong[ALG[_], A](dataDefinition: ALG[A])(
    in: IN,
    path: Path): Either[NonEmptyList[ExtractionError], Long]
  def extractBool[ALG[_], A](dataDefinition: ALG[A])(
    in: IN,
    path: Path): Either[NonEmptyList[ExtractionError], Boolean]
  def extractUuid[ALG[_], A](dataDefinition: ALG[A])(
    in: IN,
    path: Path): Either[NonEmptyList[ExtractionError], UUID] = {
    extractString(dataDefinition, classOf[UUID])(in, path).flatMap(stringToUuid(_, path))
  }
  def extractLocalDateTime[ALG[_], A](dataDefinition: ALG[A])(
    in: IN,
    path: Path): Either[NonEmptyList[ExtractionError], LocalDateTime]
  def extractLocalDate[ALG[_], A](dataDefinition: ALG[A])(
    in: IN,
    path: Path): Either[NonEmptyList[ExtractionError], LocalDate]
  def extractLocalTime[ALG[_], A](dataDefinition: ALG[A])(
    in: IN,
    path: Path): Either[NonEmptyList[ExtractionError], LocalTime]
  def extractArray[ALG[_], A](
    op: ListData[ALG, A])(in: IN, path: Path): Either[NonEmptyList[ExtractionError], Seq[IN]]
  def extractFloat[ALG[_], A](dataDefinition: ALG[A])(
    in: IN,
    path: Path): Either[NonEmptyList[ExtractionError], Float]
  def extractDouble[ALG[_], A](dataDefinition: ALG[A])(
    in: IN,
    path: Path): Either[NonEmptyList[ExtractionError], Double]
  def extractShort[ALG[_], A](dataDefinition: ALG[A])(
    in: IN,
    path: Path): Either[NonEmptyList[ExtractionError], Short]
  def extractBigDecimal[ALG[_], A](dataDefinition: ALG[A])(
    in: IN,
    path: Path): Either[NonEmptyList[ExtractionError], BigDecimal]
  def stringValue(in: IN, elementName: String): Option[String]
  def invalidValue[T](
    in: IN,
    expected: Class[T],
    path: Path): Left[NonEmptyList[ExtractionError], Nothing]

  def required[ALG[_], A](
                           coproductDataDefinition: AlgebraWithManifest[ALG,A],
                           validations: List[ValidationOp[A]],
                           f: (IN, List[String]) => Either[NonEmptyList[ExtractionError], A],
  ): (Option[IN], List[String]) => Either[NonEmptyList[ExtractionError], A] =
    (inOpt: Option[IN], path: Path) =>
      for {
        json <- inOpt
          .toRight(NonEmptyList.one(RequiredValue(path, coproductDataDefinition)))
        a <- f(json, path)
        v <- vu.validate(validations)(a, path)
      } yield a

  protected def kvpCoproduct[ALG[_], C <: Coproduct](
    co: KvpCoproduct[ALG, C],
    validator: InterchangeFormatValidator[ALG, IN])
    : (IN, Path, CoproductType) => Either[NonEmptyList[ExtractionError], C] = {
    co match {
      case nil: KvpCoNil[_] =>
        (_: IN, path: List[String], coType: CoproductType) =>
          Left(NonEmptyList.one(SumTypeError(path, s"Unexpected type value: ${coType}")))
      case co: KvpSingleValueLeft[ALG, a, r] @unchecked => {
        val fValue = validator.validate(co.kvpValue)
        val fTail = kvpCoproduct[ALG, r](co.kvpTail, validator)
        (in, path, coType) =>
          {
            if (coType == co.manifestL.runtimeClass.getSimpleName)
              fValue(Some(in), path).map(Inl(_))
            else fTail(in, path, coType).map(Inr(_))
          }
      }
    }
  }

  def kvpHList[ALG[_], H <: HList, HL <: Nat](
    group: KvpHList[ALG, H, HL],
    validator: InterchangeFormatValidator[ALG, IN])
    : (IN, List[String]) => Either[NonEmptyList[ExtractionError], H] = {
    group match {
      case nil: KvpNil[_] =>
        (_: IN, _: List[String]) =>
          Right(HNil)

      case op: KvpHListHead[ALG, H, al, h, hl, t, tl] => {
        val headInterpreter = kvpHList(op.head, validator)
        val tailInterpreter = kvpHList(op.tail, validator)

        (in: IN, path: Path) =>
          {

            Util
              .eitherMap2(headInterpreter(in, path), tailInterpreter(in, path))((l1: h, l2: t) => {
                op.prepend.apply(l1, l2)
              })
              .flatMap { l =>
                vu.validate[H](op.validations)(l, path)
              }
          }
      }

      case op: KvpConcreteTypeHead[ALG, a, ht, nt] => {
        val headInterpreter: (IN, List[String]) => Either[NonEmptyList[ExtractionError], a] =
          validatorFromSchemaWithPath(op.bonesSchema, validator)
        val tailInterpreter = kvpHList(op.tail, validator)
        (in: IN, path: Path) =>
          {
            Util
              .eitherMap2[a, ht, a :: ht](headInterpreter(in, path), tailInterpreter(in, path))(
                (l1: a, l2: ht) => {
                  op.isHCons.cons(l1, l2)
                })
              .flatMap { l =>
                vu.validate[a :: ht](op.validations)(l, path)
              }
          }
      }

      case op: KvpSingleValueHead[ALG, h, t, tl, a] => {

        val headInterpreter =
          validator.validate(op.fieldDefinition.dataDefinition)
//          determineValueDefinition(op.fieldDefinition.dataDefinition, validator)
        val tailInterpreter = kvpHList(op.tail, validator)

        (in: IN, path: Path) =>
          {

            val headPath = path :+ op.fieldDefinition.key

            val head =
              headValue(in, op.fieldDefinition, headInterpreter, headPath)
            val tailValue = tailInterpreter(in, path)

            Util
              .eitherMap2(head, tailValue)((l1, l2) => {
                op.isHCons.cons(l1, l2)
              })
              .flatMap { l =>
                vu.validate(op.validations)(l, path)
              }
          }
      }
    }
  }

  protected def isEmpty(json: IN): Boolean


}
