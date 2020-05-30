package com.bones.data

import com.bones.syntax.NoAlgebra
import com.bones.validation.ValidationDefinition.ValidationOp
import shapeless.ops.hlist.Tupler
import shapeless.{Coproduct, Generic, HList, Nat}

/**
  * If a custom algebra mixin in this trait, it's subtypes can mix in AlgToCollectionData.
  * @tparam A The type of the Wrapped Value
  */
trait HasManifest[A] {
  val manifestOfA: Manifest[A]
}
object HasManifest {
  implicit class ToCollection[ALG[_] <: HasManifest[A], A: Manifest](hm: ALG[A]) { self =>
    def list(validationOps: ValidationOp[List[A]]*) =
      ListData[ALG, A](Right(hm), validationOps.toList)

    def optional =
      OptionalKvpValueDefinition(Right(hm))
  }
}

object CustomAlgebra {
  type CustomAlgebraWithManifest[ALG[_], A] = ALG[A] with HasManifest[A]


}

import CustomAlgebra._

/** KvpValue is meant to be the 'value' of a key value pair.
  * This can be one of the pre-defined primitive 'Bones' types or a product type (to be implemented),
  * one of KvpHList, SumType or
  */
sealed abstract class KvpValue[A: Manifest] extends HasManifest[A] {
  val manifestOfA: Manifest[A] = manifest[A]
}

/** Wraps a data definition to mark the field optional */
case class OptionalKvpValueDefinition[ALG[_], B: Manifest](
  valueDefinitionOp: Either[KvpValue[B], ALG[B]])
    extends KvpValue[Option[B]] {}

/** Provides a convenient `optional` method to make data optional */
trait ToOptionalData[B] { self: KvpValue[B] =>
  private implicit val optManifestOfB: Manifest[B] = self.manifestOfA
  val optional: OptionalKvpValueDefinition[NoAlgebra, B] =
    OptionalKvpValueDefinition[NoAlgebra, B](Left(self))
}

/** Provides a convenient `optional` method to make data optional for a Custom Algebras. */
trait AlgToOptionalData[ALG[_], B, SELF <: CustomAlgebraWithManifest[ALG, B]] { self: SELF =>
  private implicit val optManifestOfB: Manifest[B] = self.manifestOfA
  val optional: OptionalKvpValueDefinition[ALG, B] = OptionalKvpValueDefinition[ALG, B](Right(self))
}

/** Provides a convenient `list` method to wrap the data in a list  */
trait ToListData[B] { self: KvpValue[B] =>
  private implicit val listManifestOfB: Manifest[B] = self.manifestOfA
  val list: ListData[NoAlgebra, B] = ListData[NoAlgebra, B](Left(self), List.empty)
}

/** Provides a convenient `list` method to wrap the data in a list for a Custom Algebras */
trait AlgToListData[ALG[_], B, SELF <: CustomAlgebraWithManifest[ALG, B]] { self: SELF =>
  private implicit val listManifestOfB: Manifest[B] = self.manifestOfA
  val list: ListData[ALG, B] = ListData[ALG, B](Right(self), List.empty)
}

/** Combines ToOptionalData and ToListData for convenience.  They are separate because
  * a ListData type can not be wrapped this way. */
trait ToCollectionData[B] extends ToOptionalData[B] with ToListData[B] { self: KvpValue[B] =>

}

/** Combines AlgToOptionalData and AlgToListData for convenience.
  * @tparam ALG This is the base GADT Trait for the Custom Algebra
  * @tparam B This is the type being wrapped by SELF
  * @tparam SELF The is the concrete data class which extends ALG
  */
trait AlgToCollectionData[ALG[_], B, SELF <: CustomAlgebraWithManifest[ALG, B]]
    extends AlgToOptionalData[ALG, B, SELF]
    with AlgToListData[ALG, B, SELF] { self: SELF =>
}

final case class EitherData[ALG[_], A: Manifest, B: Manifest](
  definitionA: Either[KvpValue[A], ALG[A]],
  definitionB: Either[KvpValue[B], ALG[B]])
    extends KvpValue[Either[A, B]]
    with ToCollectionData[Either[A, B]] {}


final case class ListData[ALG[_], T: Manifest](
  tDefinition: Either[KvpValue[T], ALG[T]],
  validations: List[ValidationOp[List[T]]])
    extends KvpValue[List[T]]
    with ToOptionalData[List[T]]


/** Represents a type where the value is an HList */
final case class KvpHListValue[ALG[_], H <: HList: Manifest, HL <: Nat](
  kvpHList: KvpHList[ALG, H, HL],
  validations: List[ValidationOp[H]])
    extends KvpValue[H]
    with ToOptionalData[H] {

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

trait BonesSchema[ALG[_], A] {
  val manifestOfA: Manifest[A]
}

/** Represents a coproduct value where the resulting type is a shapeless coproduct */
final case class KvpCoproductValue[ALG[_], C <: Coproduct: Manifest](
  kvpCoproduct: KvpCoproduct[ALG, C])
    extends KvpValue[C]
    with ToCollectionData[C]
    with ToListData[C] {}

/** Specifies a conversion to and from an HList to an A (where A is most likely a Case class) */
final case class HListConvert[ALG[_], H <: HList, N <: Nat, A: Manifest](
  from: KvpHList[ALG, H, N],
  fHtoA: H => A,
  fAtoH: A => H,
  validations: List[ValidationOp[A]])
    extends KvpValue[A]
    with ToCollectionData[A]
    with ToListData[A]
    with BonesSchema[ALG, A] {}

/** Represents a coproduct value C which is to be converted to a class represented by A */
final case class KvpCoproductConvert[ALG[_], C <: Coproduct, A: Manifest](
  from: KvpCoproduct[ALG, C],
  cToA: C => A,
  aToC: A => C,
  validations: List[ValidationOp[A]]
) extends KvpValue[A]
    with ToCollectionData[A]
    with ToListData[A]
    with BonesSchema[ALG, A]
