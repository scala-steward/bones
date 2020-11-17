package com.bones.interpreter.values

import com.bones.Util.stringToUuid
import com.bones.data.Error.ExtractionErrors
import com.bones.data.values.{JavaUtilValue, UuidData}
import com.bones.interpreter.{InterchangeFormatPrimitiveValidator, InterchangeFormatValidatorValue}

trait JavaUtilValidator[IN] extends InterchangeFormatValidatorValue[JavaUtilValue, IN] {

  val baseValidator: InterchangeFormatPrimitiveValidator[IN]

  override def validate[A](
    alg: JavaUtilValue[A]): (Option[IN], List[String]) => Either[ExtractionErrors[String], A] = {
    alg match {
      case UuidData(validations) => {
        baseValidator.required(
          alg.typeName,
          validations,
          (in, path) =>
            baseValidator.extractString(alg.typeName)(in, path).flatMap(stringToUuid(_, path))
        )
      }
    }
  }
}
