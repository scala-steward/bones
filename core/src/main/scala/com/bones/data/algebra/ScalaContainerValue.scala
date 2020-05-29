package com.bones.data.algebra

import com.bones.data.BonesSchema.ToBonesSchema
import com.bones.data.HasManifest.AlgebraWithManifest
import com.bones.data.{BonesSchema, HasManifest, KvpCoNil, KvpCoproduct, KvpHList, KvpNil}
import com.bones.validation.algebra.ScalaCoreValidation.ValidationOp
import shapeless.ops.coproduct.Inject
import shapeless.ops.hlist.Tupler
import shapeless.{Coproduct, Generic, HList, Nat}

sealed abstract class ScalaContainerValue[A: Manifest] extends HasManifest[A] {
  val manifestOfA: Manifest[A] = manifest[A]
}

//final case class RecursiveData[ALG[_],A](kvp: ALG[A])
//  extends ScalaContainerValue[A]

final case class EitherData[ALG[_], A: Manifest, B: Manifest](
  definitionA: Either[ALG[A], ALG[A]],
  definitionB: Either[ALG[B], ALG[B]])
    extends ScalaContainerValue[Either[A, B]]

final case class ListData[ALG[_], T: Manifest](
  tDefinition: ALG[T],
  validations: List[ValidationOp[List[T]]])
    extends ScalaContainerValue[List[T]]

/** Wraps a data definition to mark the field optional */
final case class OptionalKvpValueDefinition[ALG[_], B: Manifest](valueDefinitionOp: ALG[B])
    extends ScalaContainerValue[Option[B]]

/** Represents a type where the value is an HList */
final case class KvpHListValue[ALG[_], H <: HList: Manifest, HL <: Nat](
  kvpHList: KvpHList[ALG, H, HL],
  validations: List[ValidationOp[H]])
    extends ScalaContainerValue[H] {

  def convert[Z: Manifest](convertValidation: ValidationOp[Z]*)(
    implicit gen: Generic.Aux[Z, H]): HListConvert[ALG, H, HL, Z] =
    HListConvert(kvpHList, gen.from, gen.to, convertValidation.toList)

  def tupled[Tup <: Product: Manifest](tupleValidations: ValidationOp[Tup]*)(
    implicit tupler: Tupler.Aux[H, Tup],
    gen: Generic[Tup]
  ): HListConvert[ALG, H, HL, Tup] =
    HListConvert[ALG, H, HL, Tup](
      kvpHList,
      (h: H) => tupler.apply(h),
      (t: Tup) => gen.to(t).asInstanceOf[H],
      tupleValidations.toList)

}

/** Represents a coproduct value where the resulting type is a shapeless coproduct */
final case class KvpCoproductValue[ALG[_], C <: Coproduct: Manifest](
  kvpCoproduct: KvpCoproduct[ALG, C])
    extends ScalaContainerValue[C]

/** Specifies a conversion to and from an HList to an A (where A is most likely a Case class) */
final case class HListConvert[ALG[_], H <: HList, N <: Nat, A: Manifest](
  from: KvpHList[ALG, H, N],
  fHtoA: H => A,
  fAtoH: A => H,
  validations: List[ValidationOp[A]])
    extends ScalaContainerValue[A]
//    with ToBonesSchema[ALG, A] {}

/** Represents a coproduct value C which is to be converted to a class represented by A */
final case class KvpCoproductConvert[ALG[_], C <: Coproduct, A: Manifest](
  from: KvpCoproduct[ALG, C],
  cToA: C => A,
  aToC: A => C,
  validations: List[ValidationOp[A]]
) extends ScalaContainerValue[A]
//    with ToBonesSchema[ALG, A]

trait ScalaContainerValueImplicitSugar {
  implicit class HasManifestToCollections[ALG[_], A](alg: AlgebraWithManifest[ALG, A]) {
    private implicit val optManifestOfB: Manifest[A] = alg.manifestOfA

    def optional: OptionalKvpValueDefinition[ALG, A] =
      OptionalKvpValueDefinition[ALG, A](alg)

    def list: ListData[ALG, A] = ListData[ALG, A](alg, List.empty)

  }
}

trait ScalaContainerValueSugar {

  /**
    * Indicates that the data tied to this key is a list (JSON Array) type.  AllCustomAlgebras values are type
    * T and all values must pass the list of validations.
    *
    * @param dataDefinitionOp - One of the supported KvpValue types.
    * @param v List of validations each element of the list must pass to be valid.
    * @tparam T The type of each element.  Can be an EitherFieldDefinition if more than one type is expected in the list.
    * @return
    */
  def list[ALG[_], T: Manifest](
    dataDefinitionOp: ALG[T],
    v: ValidationOp[List[T]]*
  ): ListData[ALG, T] =
    ListData[ALG, T](dataDefinitionOp, v.toList)

  /** Indicates that the data tied to this key is a Date type with the specified format that must pass the specified validations. */
  def either[ALG[_], A: Manifest, B: Manifest](
    definitionA: ALG[A],
    definitionB: ALG[B]
  ): EitherData[ALG, A, B] =
    EitherData(Left(definitionA), Left(definitionB))

  /** Indicates that the data is a list of Key Value pairs */
  def kvpHList[H <: HList: Manifest, HL <: Nat, ALG[_]](
    kvpHList: KvpHList[ALG, H, HL],
    v: ValidationOp[H]*
  ): KvpHListValue[ALG, H, HL] =
    KvpHListValue(kvpHList, v.toList)

  def kvpNil[ALG[_]] = new KvpNil[ALG]()

  def kvpCoNil[ALG[_]] = new KvpCoNil[ALG]()

}

trait ScalaContainerValueSugarInjected[ALG[_] <: Coproduct] {

  /** Defines the context (the coproduct) of which we are Injecting definitions of this algebra into. */
  def scalaContainerInject[A]: Inject[ALG[A], ScalaContainerValue[A]]

  /**
    * Indicates that the data tied to this key is a list (JSON Array) type.  AllCustomAlgebras values are type
    * T and all values must pass the list of validations.
    *
    * @param dataDefinitionOp - One of the supported KvpValue types.
    * @param v List of validations each element of the list must pass to be valid.
    * @tparam T The type of each element.  Can be an EitherFieldDefinition if more than one type is expected in the list.
    * @return
    */
  def list[T: Manifest](
    dataDefinitionOp: ALG[T],
    v: ValidationOp[List[T]]*
  ): ALG[List[T]] =
    scalaContainerInject(ListData[ALG, T](dataDefinitionOp, v.toList))

  /** Indicates that the data tied to this key is a Date type with the specified format that must pass the specified validations. */
  def either[A: Manifest, B: Manifest](
    definitionA: ALG[A],
    definitionB: ALG[B]
  ): EitherData[ALG, A, B] =
    EitherData(Left(definitionA), Left(definitionB))

  /** Indicates that the data is a list of Key Value pairs */
  def kvpHList[H <: HList: Manifest, HL <: Nat, ALG[_]](
    kvpHList: KvpHList[ALG, H, HL],
    v: ValidationOp[H]*
  ): KvpHListValue[ALG, H, HL] =
    KvpHListValue(kvpHList, v.toList)

  def kvpNil[ALG[_]] = new KvpNil[ALG]()

  def kvpCoNil[ALG[_]] = new KvpCoNil[ALG]()

}
