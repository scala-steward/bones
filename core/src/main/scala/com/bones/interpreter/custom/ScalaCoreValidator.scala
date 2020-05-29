package com.bones.interpreter.custom

import cats.data.NonEmptyList
import com.bones.data.Error
import com.bones.data.algebra.ScalaCoreValue
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.InterchangeFormatValidator

trait ScalaCoreValidator[ALG[_], OUT] extends InterchangeFormatValidator[ScalaCoreValue, OUT]{

  val algValidator: InterchangeFormatValidator[ALG, OUT]

  override def validate[A](alg: ScalaCoreValue[A]): (Option[OUT], List[String]) => Either[NonEmptyList[Error.ExtractionError], A] = {
    alg match {

    }
  }
}
