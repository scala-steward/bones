package com.bones.data

import java.io.InputStream
import java.time.ZonedDateTime
import java.util.UUID

import cats.free.FreeApplicative
import com.bones.data.Error.CanNotConvert
import com.bones.data.Value.KvpGroup
import com.bones.validation.ValidationDefinition.ValidationOp
import shapeless.ops.hlist
import shapeless.ops.hlist.Length.Aux
import shapeless.ops.hlist.Split.Aux
import shapeless.ops.hlist.{Length, Prepend, Split}
import shapeless.{::, Generic, HList, HNil, Nat, Succ}


object Value {




  trait Value[A] {

    def ::[H](kvd: KeyValueDefinition[H]): KvpSingleValueHead[H, A :: HNil, Nat._1, H :: A :: HNil, Nat._2]

  }

  /**
    * This is an abstraction which describes a reference to a bunch of bytes.  For instance, a local
    * file, stream, buffer or a s3 bucket.
    */
  trait ByteReference {
    def contentType: String
    def inputStream: InputStream
  }

  object ValueDefinitionOp {

    implicit class StringToEnum(op: ValueDefinitionOp[String]) {
      def enumeration[A](enumeration: Enumeration) =
        EnumerationStringData[A](enumeration, List.empty)

      def enum[A <: Enum[A]: Manifest](enums: List[A]) =
        EnumStringData[A](enums, List.empty)

    }

  }

  /** ValueDefinitionOp is the base trait to describe a piece of data which may be
    * a single value or an HList. */
  abstract class ValueDefinitionOp[A] {
    //lift any ValueDefinition into a FreeApplicative
//    def lift: ValueDefinition[A] = ???

    def asSumType[B](
                      description: String,
                      fab: A => Either[CanNotConvert[A,B], B],
                      fba: B => A,
                      keys: List[A],
                      validations: List[ValidationOp[B]]
                    ): SumTypeData[A,B] = {
      SumTypeData[A,B](this, fab, fba, keys, validations)
    }


  }

  type ValueDefinition[A] = FreeApplicative[ValueDefinitionOp, A]

  /** Wraps a data definition to mark the field optional */
  case class OptionalValueDefinition[B](valueDefinitionOp: ValueDefinitionOp[B])
    extends ValueDefinitionOp[Option[B]] {
  }

  /** Syntactic sugar to wrap the data definition in an Optional type.
    * Also a sort of marker interface, if this is mixed in, the field is optional.
    * TODO: This should not extend ValueDefinitionOp[A]
    **/
  trait ToOptionalData[B] { self: ValueDefinitionOp[B] =>
    val optional: OptionalValueDefinition[B] = OptionalValueDefinition[B](self)
  }

  final case class BooleanData(validations: List[ValidationOp[Boolean]]) extends ValueDefinitionOp[Boolean] with ToOptionalData[Boolean]
  final case class DoubleData(validations: List[ValidationOp[Double]]) extends ValueDefinitionOp[Double] with ToOptionalData[Double]
  final case class EitherData[A, B](
      definitionA: ValueDefinitionOp[A],
      definitionB: ValueDefinitionOp[B])
    extends ValueDefinitionOp[Either[A, B]] with ToOptionalData[Either[A, B]] {
  }
  final case class IntData(validations: List[ValidationOp[Int]]) extends ValueDefinitionOp[Int] with ToOptionalData[Int]
  final case class ListData[T, L <: List[T]](tDefinition: ValueDefinitionOp[T], validations: List[ValidationOp[L]])
    extends ValueDefinitionOp[L] with ToOptionalData[L]
  final case class StringData(validations: List[ValidationOp[String]])
    extends ValueDefinitionOp[String] with ToOptionalData[String]
  final case class BigDecimalFromString(validations: List[ValidationOp[BigDecimal]])
    extends ValueDefinitionOp[BigDecimal] with ToOptionalData[BigDecimal]
  final case class ByteReferenceData(validations: List[ValidationOp[ByteReference]])
    extends ValueDefinitionOp[ByteReference] with ToOptionalData[ByteReference]

  import java.time.format.DateTimeFormatter

  final case class DateData(dateFormat: DateTimeFormatter, formatDescription: String, validations: List[ValidationOp[ZonedDateTime]])
    extends ValueDefinitionOp[ZonedDateTime] with ToOptionalData[ZonedDateTime]

  final case class UuidData(validations: List[ValidationOp[UUID]]) extends ValueDefinitionOp[UUID] with ToOptionalData[UUID]

  final case class EnumerationStringData[A](enumeration: Enumeration, validations: List[ValidationOp[A]])
    extends ValueDefinitionOp[A] with ToOptionalData[A] {
  }

  final case class EnumStringData[A <: Enum[A]](enums: List[A], validations: List[ValidationOp[A]])
    extends ValueDefinitionOp[A] with ToOptionalData[A] {
   }

  final case class KvpGroupData[H<:HList, HL<:Nat](kvpGroup: KvpGroup[H,HL], validations: List[ValidationOp[H]])
    extends ValueDefinitionOp[H] with ToOptionalData[H] {

    def convert[Z](validation: ValidationOp[Z] *)(implicit gen: Generic.Aux[Z, H]): XMapData[H,HL,Z] =
      XMapData(kvpGroup, gen.from, gen.to, validation.toList)
  }

  final case class KvpValueData[A](value: Value[A], validations: List[ValidationOp[A]])
    extends ValueDefinitionOp[A] with ToOptionalData[A]



  final case class SumTypeData[A,B](
    from: ValueDefinitionOp[A],
    fab: A => Either[CanNotConvert[A,B], B],
    fba: B => A,
    keys: List[A],
    validations: List[ValidationOp[B]]
  ) extends ValueDefinitionOp[B] {
  }

//  sealed trait KvpGroup[L <: HList, HL <: Nat] extends ValueDefinitionOp[L] with ToOptionalData[L] {
  sealed trait KvpGroup[L <: HList, HL <: Nat] {

    def convert[Z](validation: ValidationOp[Z] *)(implicit gen: Generic.Aux[Z, L]): XMapData[L,HL,Z] =
      XMapData(this, gen.from, gen.to, validation.toList)

    def convert[Z](implicit gen: Generic.Aux[Z, L]): XMapData[L,HL,Z] = convert[Z]()

    def xmap[B](f: L => B, g: B => L, validations: ValidationOp[B]*) = XMapData(this, f, g, validations.toList)

    def :::[OUT2 <: HList, OUT2L <: Nat, P <: HList, PL <: Nat](kvp: KvpGroup[P, PL])(
      implicit prepend: Prepend.Aux[P, L, OUT2],
      lengthP: Length.Aux[P, PL],
      length: Length.Aux[OUT2, OUT2L],
      split: Split.Aux[OUT2, PL, P, L]
    ): KvpGroup[OUT2, OUT2L]

    def ::[H](v: KeyValueDefinition[H]): KvpSingleValueHead[H, L, HL, H :: L, Succ[HL]]

    def optional = OptionalKvpGroup[L,HL](this)
  }

  final case class OptionalKvpGroup[H<:HList, HL<:Nat](kvpGroup: KvpGroup[H,HL])
    extends KvpGroup[Option[H]::HNil, Nat._1] {
    override def :::[OUT2 <: HList, OUT2L <: Nat, P <: HList, PL <: Nat](kvp: KvpGroup[P, PL])(
      implicit prepend: hlist.Prepend.Aux[P, Option[H] :: HNil, OUT2],
      lengthP: Length.Aux[P, PL],
      length: Length.Aux[OUT2, OUT2L],
      split: Split.Aux[OUT2, PL, P, Option[H] :: HNil]
    ): KvpGroup[OUT2, OUT2L] = ???

    override def ::[HH](v: KeyValueDefinition[HH]):
      KvpSingleValueHead[HH, Option[H] :: HNil, Nat._1, HH :: Option[H] :: HNil, Succ[Nat._1]] = ???
  }

  /** Specifies that we have simple functions from A to B and B to A for reverse mapping.
    */
  final case class XMapData[A<:HList,AL<:Nat,B](from: KvpGroup[A,AL], fab: A => B, fba: B => A, validations: List[ValidationOp[B]])
    extends KvpGroup[B :: HNil, Nat._1] with Value[B]{ thisBase =>

    override def :::[OUT2 <: HList, OUT2L <: Nat, P <: HList, PL <: Nat](kvp: KvpGroup[P, PL])(
      implicit prepend: Prepend.Aux[P, B :: HNil, OUT2],
      lengthP: Length.Aux[P, PL],
      length: Length.Aux[OUT2, OUT2L],
      split: Split.Aux[OUT2, PL, P, B :: HNil]
    ): KvpGroup[OUT2, OUT2L] =
      KvpGroupHead[OUT2, OUT2L, P, PL, B :: HNil, Nat._1](kvp, this, prepend, split, List.empty)

    override def ::[H](v: KeyValueDefinition[H]): KvpSingleValueHead[H, B :: HNil, Nat._1, H :: B :: HNil, Nat._2] =
      KvpSingleValueHead(v, List.empty, this)
  }

//  case class OptionalKvpGroup[L<:HList,HL<:Nat](kvpGroup: KvpGroup[L,HL]) extends KvpGroup[Option[L]]


  /**
    */
  object KvpNil extends KvpGroup[HNil, Nat._0] {

    override def :::[OUT2 <: HList, OUT2L <: Nat, P <: HList, PL <: Nat](kvp: KvpGroup[P, PL])(
      implicit prepend: hlist.Prepend.Aux[P, HNil, OUT2],
      lengthP: Length.Aux[P, PL],
      length: Length.Aux[OUT2, OUT2L],
      split: Split.Aux[OUT2, PL, P, HNil]): KvpGroup[OUT2, OUT2L] =
      KvpGroupHead[OUT2, OUT2L, P, PL, HNil, Nat._0](kvp, KvpNil, prepend, split, List.empty)

    override def ::[H](v: KeyValueDefinition[H]): KvpSingleValueHead[H, HNil, Nat._0, H :: HNil, Succ[Nat._0]] =
      KvpSingleValueHead(v, List.empty, this)
  }

  final case class KvpSingleValueHead[H, T <: HList, TL <: Nat, OUT <: H :: T, OUTL <: Nat](
    fieldDefinition: KeyValueDefinition[H],
    validations: List[ValidationOp[OUT]],
    tail: KvpGroup[T, TL]
  ) extends KvpGroup[OUT, Succ[TL]] {



    /**
      *
      * When we combine groups, we want to keep the validations separate, but we want to combine the result.
      *
      * @param kvp The Group to append to this KvpGroup
      * @tparam OUT2 New HList which combines L (from this) and P (from others)
      * @tparam P The HList output type of kvp
      */
    override def :::[OUT2 <: HList, OUT2L <: Nat, P <: HList, PL <: Nat](kvp: KvpGroup[P, PL])(
      implicit prepend: hlist.Prepend.Aux[P, OUT, OUT2],
      lengthP: Length.Aux[P, PL],
      length: Length.Aux[OUT2, OUT2L],
      split: Split.Aux[OUT2, PL, P, OUT]): KvpGroup[OUT2, OUT2L] =
      KvpGroupHead[OUT2, OUT2L, P, PL, OUT, Succ[TL]](kvp, this, prepend, split, List.empty)

    override def ::[H](v: KeyValueDefinition[H]): KvpSingleValueHead[H, OUT, Succ[TL], H :: OUT, Succ[Succ[TL]]] =
      KvpSingleValueHead[H, OUT, Succ[TL], H :: OUT, Succ[Succ[TL]]](v, List.empty, this)

    def validate(v: ValidationOp[OUT]): KvpSingleValueHead[H,T,TL,OUT,OUTL] = this.copy(validations = v :: validations)
  }

  /** This is a group of KvpGroup that are grouped and the validations match the entire group.  */
  final case class KvpGroupHead[OUT <: HList, OUTL <: Nat, H <: HList, HL<: Nat, T <: HList, TL <: Nat](
    head: KvpGroup[H, HL],
    tail: KvpGroup[T, TL],
    prepend : Prepend.Aux[H, T, OUT],
    split : Split.Aux[OUT, HL, H, T], // analogous: Split.Aux[prepend.OUT,HL,H,T] with lpLength: Length.Aux[H,HL],
    validations: List[ValidationOp[OUT]]
  ) extends KvpGroup[OUT, OUTL] {
    /**
      *
      * When we combine groups, we want to keep the validations separete, but we want to combine the result.
      *
      * @param kvp The KvpGroup to append to this group.
      * @tparam OUT2 New HList which combines L (from this) and P (from others)
      * @tparam P The HList output type of the kvp group we are appending.
      */
    override def :::[OUT2 <: HList, OUT2L <: Nat, P <: HList, PL <: Nat](kvp: KvpGroup[P, PL])(
      implicit prepend: Prepend.Aux[P, OUT, OUT2],
      lengthP: Length.Aux[P, PL],
      length: Length.Aux[OUT2, OUT2L],
      split: Split.Aux[OUT2, PL, P, OUT]
    ): KvpGroup[OUT2, OUT2L] =
      KvpGroupHead[OUT2, OUT2L, P, PL, OUT, OUTL](kvp, this, prepend, split, List.empty)

    override def ::[P](kvd: KeyValueDefinition[P]): KvpSingleValueHead[P, OUT, OUTL, P :: OUT, Succ[OUTL]] =
      KvpSingleValueHead[P, OUT, OUTL, P :: OUT, Succ[OUTL]](kvd, List.empty, this)

    def validate(v: ValidationOp[OUT]): KvpGroupHead[OUT,OUTL, H, HL, T, TL] = this.copy(validations = v :: validations)

  }

}
