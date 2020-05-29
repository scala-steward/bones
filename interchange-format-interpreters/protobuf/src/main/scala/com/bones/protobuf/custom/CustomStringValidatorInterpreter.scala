package com.bones.protobuf.custom

import com.bones.data.algebra.CustomStringValue
import com.bones.protobuf.ProtobufSequentialValidatorInterpreter.{
  CustomValidatorInterpreter,
  ExtractFromProto,
  stringData
}

object CustomStringValidatorInterpreter extends CustomValidatorInterpreter[CustomStringValue] {

  override def extractFromProto[A](alg: CustomStringValue[A]): ExtractFromProto[A] = {
    stringData[CustomStringValue](
      Right(alg.asInstanceOf[CustomStringValue[String]]),
      alg.customValidation :: alg.validations)
      .asInstanceOf[ExtractFromProto[A]]
  }
}
