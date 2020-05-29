package com.bones.data.algebra

import com.bones.data.HasManifest.AlgebraWithManifest
import com.bones.data._
import com.bones.validation.algebra.ScalaCoreValidation.{BigDecimalValidation, ByteValidation, CharValidation, DoubleValidation, EnumerationValidation, FloatValidation, IntValidation, LongValidation, ShortValidation, StringValidation, ValidationOp}
import shapeless.ops.coproduct.Inject
import shapeless.ops.hlist.Tupler
import shapeless.{Coproduct, Generic, HList, Nat}

sealed abstract class ScalaCoreValue[A: Manifest] extends HasManifest[A] {
  val manifestOfA: Manifest[A] = manifest[A]
}

final case class IntData(validations: List[ValidationOp[Int]]) extends ScalaCoreValue[Int]

final case class LongData(validations: List[ValidationOp[Long]]) extends ScalaCoreValue[Long]

/** Schema type for Boolean Data */
final case class BooleanData(validations: List[ValidationOp[Boolean]])
    extends ScalaCoreValue[Boolean]

final case class ShortData(validations: List[ValidationOp[Short]]) extends ScalaCoreValue[Short]

final case class StringData(validations: List[ValidationOp[String]]) extends ScalaCoreValue[String]

final case class FloatData(validations: List[ValidationOp[Float]]) extends ScalaCoreValue[Float]

final case class DoubleData(validations: List[ValidationOp[Double]]) extends ScalaCoreValue[Double]

final case class BigDecimalData(validations: List[ValidationOp[BigDecimal]])
    extends ScalaCoreValue[BigDecimal]

/** base64-encoded characters, for example,
  * @example "U3dhZ2dlciByb2Nrcw=="
  * */
final case class ByteArrayData(validations: List[ValidationOp[Array[Byte]]])
    extends ScalaCoreValue[Array[Byte]]

final case class EnumerationData[E <: Enumeration, V: Manifest](
  enumeration: E,
  validations: List[ValidationOp[V]]
) extends ScalaCoreValue[V]



trait ScalaCoreValidationSugar {

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

  def ev[E <: Enumeration] = EnumerationValidation[E]

}

trait ScalaCoreValueSugar extends ScalaCoreValidationSugar {

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
  def enumeration[E <: Enumeration, V: Manifest](
    e: E,
    validationOp: ValidationOp[V]*
  ): EnumerationData[E, V] =
    EnumerationData[E, V](e, validationOp.toList)

  /** Indicates that the data tied to the value is an Array of Bytes */
  def byteArray(v: ValidationOp[Array[Byte]]*): ByteArrayData = ByteArrayData(v.toList)

  /** Alias for byte array without validations */
  val byteArray: ByteArrayData = byteArray()

}

trait ScalaCoreValueSugarInjected[ALG[_] <: Coproduct] {

  /** Defines the context (the coproduct) of which we are Injecting definitions of this algebra into. */
  def scalaCoreInject[A]: Inject[ALG[A], ScalaCoreValue[A]]

  /** Indicates that the data tied to this key is a String type that must pass the specified validations */
  def string(validationOp: ValidationOp[String]*): ALG[String] =
    scalaCoreInject(StringData(validationOp.toList))

  /** Alias for string without validations. */
  val string: ALG[String] = string()

  /* Indicates that the data tied to this value is a Float */
  def float(f: ValidationOp[Float]*): ALG[Float] =
    scalaCoreInject(FloatData(f.toList))

  /** Alias for float without validations. */
  val float: ALG[Float] = float()

  /** Indicates that the data tied to this value is a short */
  def short(f: ValidationOp[Short]*): ALG[Short] =
    scalaCoreInject(ShortData(f.toList))

  /** Alias for short without validations */
  val short: ALG[Short] = short()

  /** Indicates that the data tied to this value is a double */
  def double(f: ValidationOp[Double]*): ALG[Double] =
    scalaCoreInject(DoubleData(f.toList))

  /** Alias for double without validations */
  val double: ALG[Double] = double()

  /** Indicates the data tied to this Value is an Int */
  def int(f: ValidationOp[Int]*): ALG[Int] =
    scalaCoreInject(IntData(f.toList))

  /** Alias for int without any validations */
  val int: ALG[Int] = int()

  /** Indicates that the data tied to this key is an Int type that must pass the specified validations */
  def long(f: ValidationOp[Long]*): ALG[Long] =
    scalaCoreInject(LongData(f.toList))

  /** Alias for long without validations. */
  val long: ALG[Long] = long()


  /** Indicates that the data tied to this key is an boolean type that must pass the specified validations. */
  def boolean(f: ValidationOp[Boolean]*): ALG[Boolean] =
    scalaCoreInject(BooleanData(f.toList))

  val boolean: ALG[Boolean] = boolean()

  /** Indicates that the data tied to this key is a BigDecimal that must pass the specified validations. */
  def bigDecimal(v: ValidationOp[BigDecimal]*): ALG[BigDecimal] =
    scalaCoreInject(BigDecimalData(v.toList))

  /** Alias for bigDecimal without validations */
  val bigDecimal: ALG[BigDecimal] = bigDecimal()

  /** Expecting the type to be a Scala style enumeration
    *
    * @param e The base enumeration type.
    * @tparam E The enumeration
    */
  def enumeration[E <: Enumeration, V: Manifest](
    e: E,
    validationOp: ValidationOp[V]*
  ): ALG[V] =
    scalaCoreInject(EnumerationData[E, V](e, validationOp.toList))

  /** Indicates that the data tied to the value is an Array of Bytes */
  def byteArray(v: ValidationOp[Array[Byte]]*): ALG[Array[Byte]] =
    scalaCoreInject(ByteArrayData(v.toList))

  /** Alias for byte array without validations */
  val byteArray: ALG[Array[Byte]] = byteArray()

}
