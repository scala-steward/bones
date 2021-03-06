package com.bones.data.values

import com.bones.PrimitiveValue
import com.bones.validation.ValidationDefinition
import com.bones.validation.ValidationDefinition._
import shapeless.Coproduct
import shapeless.ops.coproduct.Inject

sealed abstract class ScalaCoreValue[A] extends PrimitiveValue[A] {
  val validations: List[ValidationOp[A]]
}

/** Schema type for Boolean Data */
final case class BooleanData(validations: List[ValidationOp[Boolean]])
    extends ScalaCoreValue[Boolean] {
  override val typeName: String = "Boolean"
}

final case class IntData(validations: List[ValidationOp[Int]]) extends ScalaCoreValue[Int] {
  override val typeName: String = "Int"
}

final case class LongData(validations: List[ValidationOp[Long]]) extends ScalaCoreValue[Long] {
  override val typeName: String = "Long"
}

final case class ShortData(validations: List[ValidationOp[Short]]) extends ScalaCoreValue[Short] {
  override val typeName: String = "Short"
}

final case class StringData(validations: List[ValidationOp[String]])
    extends ScalaCoreValue[String] {
  override val typeName: String = "String"
}

final case class FloatData(validations: List[ValidationOp[Float]]) extends ScalaCoreValue[Float] {
  override val typeName: String = "Float"
}

final case class DoubleData(validations: List[ValidationOp[Double]])
    extends ScalaCoreValue[Double] {
  override val typeName: String = "Double"
}

final case class BigDecimalData(validations: List[ValidationOp[BigDecimal]])
    extends ScalaCoreValue[BigDecimal] {
  override val typeName: String = "BigDecimal"
}

/** base64-encoded characters, for example,
  * @example "U3dhZ2dlciByb2Nrcw=="
  * */
final case class ByteArrayData(validations: List[ValidationOp[Array[Byte]]])
    extends ScalaCoreValue[Array[Byte]] {
  override val typeName: String = "Array[Byte]"
}

final case class EnumerationData[E <: Enumeration: Manifest, V](
  enumeration: E,
  validations: List[ValidationOp[V]]
) extends ScalaCoreValue[V] {

  override val typeName: String = "Enumeration"
}

trait BaseScalaCoreInterpreter[OUT] {

  def matchScalaCoreValue[A](alg: ScalaCoreValue[A]): OUT = {
    alg match {
      case bd: BooleanData           => boolDataToOut(bd)
      case id: IntData               => intDataToOut(id)
      case ld: LongData              => longDataToOut(ld)
      case sd: ShortData             => shortDataToOut(sd)
      case sd: StringData            => stringDataToOut(sd)
      case fd: FloatData             => floatDataToOut(fd)
      case dd: DoubleData            => doubleDataToOut(dd)
      case bd: BigDecimalData        => bigDecimalToOut(bd)
      case ba: ByteArrayData         => byteArrayToOut(ba)
      case en: EnumerationData[e, v] => enumerationToOut(en)
    }
  }

  def boolDataToOut(booleanData: BooleanData): OUT
  def intDataToOut(intData: IntData): OUT
  def longDataToOut(longData: LongData): OUT
  def shortDataToOut(shortData: ShortData): OUT
  def stringDataToOut(stringData: StringData): OUT
  def floatDataToOut(floatData: FloatData): OUT
  def doubleDataToOut(doubleData: DoubleData): OUT
  def bigDecimalToOut(bigDecimalData: BigDecimalData): OUT
  def byteArrayToOut(byteArrayData: ByteArrayData): OUT
  def enumerationToOut[A](enumerationData: EnumerationData[_, A]): OUT

}

trait ScalaCoreValidation {

  /** sv = String Validation */
  val sv: ValidationDefinition.StringValidation.type = StringValidation

  /** lv = Long validation */
  val lv: ValidationDefinition.LongValidation.type = LongValidation

  /** iv = Int validation */
  val iv: ValidationDefinition.IntValidation.type = IntValidation

  /** bdv = Big Decimal Validation */
  val bdv: ValidationDefinition.BigDecimalValidation.type = BigDecimalValidation

  /** fv = Float Validation */
  val fv: ValidationDefinition.FloatValidation.type = FloatValidation

  /** cv = Char Validation */
  val cv: ValidationDefinition.CharValidation.type = CharValidation

  /** bv = Byte Validation */
  val bv: ValidationDefinition.ByteValidation.type = ByteValidation

  /** shv = Short Validation */
  val shv: ValidationDefinition.ShortValidation.type = ShortValidation

  /** dv = double validation */
  val dv: ValidationDefinition.DoubleValidation.type = DoubleValidation

  def ev[E <: Enumeration]: EnumerationValidation[E] = EnumerationValidation[E]()
}

object ScalaCoreSugarInstance extends ScalaCoreSugar

trait ScalaCoreSugar extends ScalaCoreValidation {

  /** Indicates that the data tied to this key is a String type that must pass the specified validations */
  def string(validationOp: ValidationOp[String]*): StringData =
    StringData(validationOp.toList)

  /** Alias for string without validations. */
  val string: StringData = string()

  /* Indicates that the data tied to this value is a Float */
  def float(f: ValidationOp[Float]*): FloatData = FloatData(f.toList)

  /** Alias for float without validations. */
  val float: FloatData = float()

  /** Indicates that the data tied to this value is a short */
  def short(f: ValidationOp[Short]*): ShortData = ShortData(f.toList)

  /** Alias for short without validations */
  val short: ShortData = short()

  /** Indicates that the data tied to this value is a double */
  def double(f: ValidationOp[Double]*): DoubleData = DoubleData(f.toList)

  /** Alias for double without validations */
  val double: DoubleData = double()

  /** Indicates the data tied to this Value is an Int */
  def int(f: ValidationOp[Int]*): IntData = IntData(f.toList)

  /** Alias for int without any validations */
  val int: IntData = int()

  /** Indicates that the data tied to this key is an Int type that must pass the specified validations */
  def long(f: ValidationOp[Long]*): LongData = LongData(f.toList)

  /** Alias for long without validations. */
  val long: LongData = long()

  /** Indicates that the data tied to this key is an boolean type that must pass the specified validations. */
  def boolean(f: ValidationOp[Boolean]*): BooleanData = BooleanData(f.toList)

  val boolean: BooleanData = boolean()

  /** Indicates that the data tied to this key is a BigDecimal that must pass the specified validations. */
  def bigDecimal(v: ValidationOp[BigDecimal]*): BigDecimalData = BigDecimalData(v.toList)

  /** Alias for bigDecimal without validations */
  val bigDecimal: BigDecimalData = bigDecimal()

  /** Expecting the type to be a Scala style enumeration
    *
    * @param e The base enumeration type.
    * @tparam E The enumeration
    */
  def enumeration[E <: Enumeration: Manifest, V](
    e: E,
    validationOp: ValidationOp[V]*
  ): EnumerationData[E, V] =
    EnumerationData[E, V](e, validationOp.toList)

  /** Indicates that the data tied to the value is an Array of Bytes */
  def byteArray(v: ValidationOp[Array[Byte]]*): ByteArrayData = ByteArrayData(v.toList)

  /** Alias for byte array without validations */
  val byteArray: ByteArrayData = byteArray()

}

trait ScalaCoreInjectedSugar[ALG[_] <: Coproduct] extends ScalaCoreValidation {
  def scalaCoreInjected[A]: Inject[ALG[A], ScalaCoreValue[A]]

  /** Indicates that the data tied to this key is a String type that must pass the specified validations */
  def string(validationOp: ValidationOp[String]*): ALG[String] =
    scalaCoreInjected(StringData(validationOp.toList))

  /** Alias for string without validations. */
  def string: ALG[String] = string()

  /* Indicates that the data tied to this value is a Float */
  def float(f: ValidationOp[Float]*): ALG[Float] =
    scalaCoreInjected(FloatData(f.toList))

  /** Alias for float without validations. */
  def float: ALG[Float] = float()

  /** Indicates that the data tied to this value is a short */
  def short(f: ValidationOp[Short]*): ALG[Short] =
    scalaCoreInjected(ShortData(f.toList))

  /** Alias for short without validations */
  def short: ALG[Short] = short()

  /** Indicates that the data tied to this value is a double */
  def double(f: ValidationOp[Double]*): ALG[Double] =
    scalaCoreInjected(DoubleData(f.toList))

  /** Alias for double without validations */
  def double: ALG[Double] = double()

  /** Indicates the data tied to this Value is an Int */
  def int(f: ValidationOp[Int]*): ALG[Int] =
    scalaCoreInjected(IntData(f.toList))

  /** Alias for int without any validations */
  def int: ALG[Int] = int()

  /** Indicates that the data tied to this key is an Int type that must pass the specified validations */
  def long(f: ValidationOp[Long]*): ALG[Long] =
    scalaCoreInjected(LongData(f.toList))

  /** Alias for long without validations. */
  def long: ALG[Long] = long()

  /** Indicates that the data tied to this key is an boolean type that must pass the specified validations. */
  def boolean(f: ValidationOp[Boolean]*): ALG[Boolean] =
    scalaCoreInjected(BooleanData(f.toList))

  def boolean: ALG[Boolean] = boolean()

  /** Indicates that the data tied to this key is a BigDecimal that must pass the specified validations. */
  def bigDecimal(v: ValidationOp[BigDecimal]*): ALG[BigDecimal] =
    scalaCoreInjected(BigDecimalData(v.toList))

  /** Alias for bigDecimal without validations */
  def bigDecimal: ALG[BigDecimal] = bigDecimal()

  /** Expecting the type to be a Scala style enumeration
    *
    * @param e The base enumeration type.
    * @tparam E The enumeration
    */
  def enumeration[E <: Enumeration: Manifest, V](
    e: E,
    validationOp: ValidationOp[V]*
  ): ALG[V] =
    scalaCoreInjected(EnumerationData[E, V](e, validationOp.toList))

  /** Indicates that the data tied to the value is an Array of Bytes */
  def byteArray(v: ValidationOp[Array[Byte]]*): ALG[Array[Byte]] =
    scalaCoreInjected(ByteArrayData(v.toList))

  /** Alias for byte array without validations */
  def byteArray: ALG[Array[Byte]] = byteArray()

}
