package com.bones.interpreter.values

import com.bones.data.values._
import com.bones.interpreter.{
  InterchangeFormatEncoderValue,
  InterchangeFormatPrimitiveEncoder,
  KvpInterchangeFormatEncoderInterpreter
}

trait ScalaCoreEncoder[OUT] extends InterchangeFormatEncoderValue[ScalaCoreValue, OUT] {
  val defaultEncoder: InterchangeFormatPrimitiveEncoder[OUT]

  override def encode[A](alg: ScalaCoreValue[A]): A => OUT =
    alg match {
      case _: BooleanData    => defaultEncoder.booleanToOut
      case _: StringData     => defaultEncoder.stringToOut
      case _: IntData        => defaultEncoder.intToOut
      case _: LongData       => defaultEncoder.longToOut
      case _: FloatData      => defaultEncoder.floatToOut
      case _: DoubleData     => defaultEncoder.doubleToOut
      case _: ShortData      => defaultEncoder.shortToOut
      case _: BigDecimalData => defaultEncoder.bigDecimalToOut
      case _: ByteArrayData  => defaultEncoder.byteArrayToOut
      case e: EnumerationData[e, a] => { enum =>
        defaultEncoder.stringToOut(enum.toString)
      }

    }
}
