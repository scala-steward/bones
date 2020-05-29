package com.bones.interpreter.custom

import java.util.Base64

import cats.data.NonEmptyList
import com.bones.Util.stringToEnumeration
import com.bones.data.Error
import com.bones.data.Error.{CanNotConvert, ExtractionError, RequiredValue, SumTypeError, WrongTypeError}
import com.bones.data.algebra.{EitherData, OptionalKvpValueDefinition, ScalaContainerValue}
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.InterchangeFormatValidator

import scala.util.Try

trait ScalaContainerValidator[ALG[_], OUT] extends InterchangeFormatValidator[ScalaContainerValue, OUT]{

  val algValidator: InterchangeFormatValidator[ALG, OUT]

  override def validate[A](alg: ScalaContainerValue[A]):
    (Option[OUT], List[String]) => Either[NonEmptyList[Error.ExtractionError], A] = {

    alg match {
      case op: OptionalKvpValueDefinition[ALG, a] @unchecked =>
        val applied = determineValueDefinition(op.valueDefinitionOp, extendedValidator)
        (in: Option[IN], path: List[String]) =>
          in match {
            case None                  => Right(None)
            case Some(n) if isEmpty(n) => Right(None)
            case some @ Some(json)     => applied(some, path).map(Some(_))
          }
      case op: StringData =>
        required(Left(op), op.validations, extractString(Left(fgo), classOf[String]))
      case id: IntData =>
        required(Left(id), id.validations, extractInt(Left(fgo)))
      case op: LongData =>
        required(Left(op), op.validations, extractLong(Left(fgo)))
      case op: BooleanData =>
        required(Left(op), op.validations, extractBool(Left(fgo)))
      case op: UuidData =>
        required(Left(op), op.validations, extractUuid(Left(fgo)))
      case op @ LocalDateTimeData(validations) =>
        required(Left(op), validations, extractLocalDateTime(Left(fgo)))
      case op @ LocalDateData(validations) =>
        required(Left(op), validations, extractLocalDate(Left(fgo)))
      case op @ LocalTimeData(validations) =>
        required(Left(op), validations, extractLocalTime(Left(fgo)))
      case op @ ByteArrayData(validations) =>
        val decoder = Base64.getDecoder
        (inOpt: Option[IN], path: Path) =>
          for {
            in <- inOpt.toRight[NonEmptyList[ExtractionError]](
              NonEmptyList.one(RequiredValue(path, Left(op))))
            str <- extractString(Left(fgo), classOf[Array[Byte]])(in, path)
            arr <- Try {
              decoder.decode(str)
            }.toEither.left.map(thr =>
              NonEmptyList.one(CanNotConvert(path, str, classOf[Array[Byte]], None)))
          } yield arr

      case ed: EitherData[ALG, a, b] @unchecked =>
        val optionalA = determineValueDefinition(ed.definitionA, extendedValidator)
        val optionalB = determineValueDefinition(ed.definitionB, extendedValidator)
        (in: Option[IN], path: Path) =>
        {
          optionalA(in, path) match {
            case Left(err) =>
              val nonWrongTypeError = err.toList.filter {
                case WrongTypeError(_, _, _, _) => false
                case RequiredValue(_, _)         => false
                case CanNotConvert(_, _, _, _)  => false
                case _                          => true
              }
              if (nonWrongTypeError.isEmpty) {
                optionalB(in, path) match {
                  case Right(b) => Right(Right(b))
                  case Left(err) => {
                    Left(NonEmptyList.one(RequiredValue(path, Left(ed))))
                  }
                }
              } else {
                Left(err)
              }
            case Right(a) => Right(Left(a))
          }
        }
      case op: ListData[ALG, t] @unchecked =>
        val valueF = determineValueDefinition(op.tDefinition, extendedValidator)

        def appendArrayInex(path: Path, index: Int): List[String] = {
          if (path.length == 0) path
          else
            path.updated(path.length - 1, path(path.length - 1) + s"[${index}]")
        }

        def traverseArray(
                           arr: Seq[IN],
                           path: Path): Either[NonEmptyList[ExtractionError], List[t]] = {
          val arrayApplied: Seq[Either[NonEmptyList[ExtractionError], t]] =
            arr.zipWithIndex.map(jValue =>
              valueF(Some(jValue._1), appendArrayInex(path, jValue._2)))

          arrayApplied
            .foldLeft[Either[NonEmptyList[ExtractionError], List[t]]](Right(List.empty))((b, v) =>
              (b, v) match {
                case (Right(a), Right(i)) => Right(a :+ i)
                case (Left(a), Left(b))   => Left(a ::: b)
                case (Left(x), _)         => Left(x)
                case (_, Left(x))         => Left(x)
              })
        }

        (inOpt: Option[IN], path: Path) =>
        {
          for {
            in <- inOpt.toRight(NonEmptyList.one(RequiredValue(path, Left(op))))
            arr <- extractArray(op)(in, path)
            listOfIn <- traverseArray(arr, path)
          } yield listOfIn
        }
      case fd: FloatData =>
        required(Left(fd), fd.validations, extractFloat(Left(fgo)))
      case dd: DoubleData =>
        required(Left(dd), dd.validations, extractDouble(Left(fgo)))
      case sd: ShortData =>
        required(Left(sd), sd.validations, extractShort(Left(fgo)))
      case op: BigDecimalData =>
        required(Left(op), op.validations, extractBigDecimal(Left(fgo)))
      case op: EnumerationData[e, A] =>
        (inOpt: Option[IN], path: Path) =>
          for {
            in <- inOpt.toRight[NonEmptyList[ExtractionError]](
              NonEmptyList.one(RequiredValue(path, Left(op))))
            str <- extractString(Left(fgo), op.manifestOfA.runtimeClass)(in, path)
            enum <- stringToEnumeration[e, A](str, path, op.enumeration.asInstanceOf[e])(
              op.manifestOfA)
          } yield enum.asInstanceOf[A]

      case op: KvpHListValue[ALG, h, hl] @unchecked => {
        val fg: (IN, List[String]) => Either[NonEmptyList[ExtractionError], h] =
          kvpHList(op.kvpHList, extendedValidator)
        (jsonOpt: Option[IN], path: Path) =>
        {
          jsonOpt match {
            case Some(json) =>
              fg(json, path)
                .flatMap(res => vu.validate[h](op.validations)(res, path))
                .map(_.asInstanceOf[A])
            case None => Left(NonEmptyList.one(RequiredValue(path, Left(op))))
          }
        }
      }
      case co: KvpCoproductValue[ALG, c] @unchecked => {
        val fCo = kvpCoproduct(co.kvpCoproduct, extendedValidator)
        (jsonOpt: Option[IN], path: Path) =>
        {
          jsonOpt match {
            case Some(json) => {
              stringValue(json, coproductTypeKey) match {
                case Some(coType) => fCo(json, path, coType).map(_.asInstanceOf[A])
                case None =>
                  Left(
                    NonEmptyList.one(
                      SumTypeError(path, s"Missing parameter ${coproductTypeKey}")))
              }

            }
            case None => Left(NonEmptyList.one(RequiredValue(path, Left(co))))
          }
        }
      }
      case x: HListConvert[ALG, a, al, A] @unchecked => {
        val kvp = kvpHList(x.from, extendedValidator)
        (jOpt: Option[IN], path: Path) =>
          jOpt match {
            case None    => Left(NonEmptyList.one(RequiredValue(path, Left(x))))
            case Some(j) => kvp(j, path).map(x.fHtoA(_))
          }
      }
      case co: KvpCoproductConvert[ALG, a, c] @unchecked => {
        val fCo = kvpCoproduct(co.from, extendedValidator)
        (jOpt: Option[IN], path: Path) =>
          jOpt match {
            case None => Left(NonEmptyList.one(RequiredValue(path, Left(co))))
            case Some(j) => {
              stringValue(j, coproductTypeKey) match {
                case None =>
                  Left(NonEmptyList.one(RequiredValue(coproductTypeKey :: path, Left(co))))
                case Some(coproductType) => fCo(j, path, coproductType).map(co.cToA(_))
              }
            }
          }
      }
    }

  }

}
