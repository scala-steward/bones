package com.bones

import java.time.format.DateTimeFormatter

import _root_.argonaut.Json
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.DateToStringEncoder
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.StringToDateValidator

package object argonaut {

  /** An implementation of an Argonaut Encoder and and Validator using Standard ISO DATE string
    * format for serializing.
    * Implement both Encoder and Validator to ensure consistent data formats and coproductTypeKey */
  object IsoArgonautEncoderAndValidatorInterpreter
      extends ArgonautEncoderInterpreter
      with ArgonautValidatorInterpreter {

    override val coproductTypeKey: String = "type"
  }

}
