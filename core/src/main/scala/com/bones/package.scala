package com

import com.bones.data.{KeyValueDefinitionSugar, Sugar}
import com.bones.validation.ValidationDefinition._

/**
  * Collect all functionality here so one only needs to specify one import statement: 'com.bones.syntax._'
  */
package object bones {

  /** So we can just import com.bones.syntax._ */
  object syntax extends Sugar with KeyValueDefinitionSugar {

    /** This type is useful when we are not dealing with a custom algebra. */
    type NoAlgebra[A] = Nothing

    /** sv = String Validation */
    val sv = StringValidation

    /** lv = Long validation */
    val lv = LongValidation

    /** iv = Int validation */
    val iv = IntValidation

    /** bdv = Big Decimal Validation */
    val bdv = BigDecimalValidation

    /** fv = Float Validation */
    val fv = FloatValidation

    /** cv = Char Validation */
    val cv = CharValidation

    /** bv = Byte Validation */
    val bv = ByteValidation

    /** shv = Short Validation */
    val shv = ShortValidation

    /** dv = double validation */
    val dv = DoubleValidation

    /** ldtv = LocalDateTimeValidationInstances */
    val ldtv = LocalDateTimeValidationInstances

    /** ldv = LocalDateValidationInstances */
    val ldv = LocalDateValidationInstances

  }

}
