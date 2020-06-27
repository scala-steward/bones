package com.bones.interpreter.values

import java.util.UUID

import com.bones.data.values.{JavaUtilValue, UuidData}
import com.bones.interpreter.{InterchangeFormatEncoder, KvpInterchangeFormatEncoderInterpreter}

trait JavaUtilEncoder[OUT] extends InterchangeFormatEncoder[JavaUtilValue, OUT] {

  val defaultEncoder: KvpInterchangeFormatEncoderInterpreter[OUT]

  override def encode[A](alg: JavaUtilValue[A]): A => OUT = alg match {
    case UuidData(_) =>
      (uuid: UUID) =>
        defaultEncoder.stringToOut.apply(uuid.toString)

  }
}
