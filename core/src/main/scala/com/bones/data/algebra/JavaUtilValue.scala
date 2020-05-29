package com.bones.data.algebra

import java.util.UUID

import com.bones.data.HasManifest
import com.bones.validation.algebra.ScalaCoreValidation.ValidationOp
import shapeless.Coproduct
import shapeless.ops.coproduct.Inject

sealed abstract class JavaUtilValue[A: Manifest] extends HasManifest[A] {
  val manifestOfA: Manifest[A] = manifest[A]
}

final case class UuidData(validations: List[ValidationOp[UUID]])
  extends JavaUtilValue[UUID]


trait JavaUtilValueSugar {
  /** Indicates that the data tied to this key is a UUID type that must pass the specified validations. */
  def uuid(v: ValidationOp[UUID]*): UuidData = UuidData(v.toList)

  /** Alias for UUID without validations */
  val uuid: UuidData = UuidData(List.empty)
}

trait JavaUtilValueSugarInjected[ALG[_] <: Coproduct] {

  /** Defines the context (the coproduct) of which we are Injecting definitions of this algebra into. */
  def javaUtilInject[A]: Inject[ALG[A], JavaUtilValue[A]]

  /** Indicates that the data tied to this key is a UUID type that must pass the specified validations. */
  def uuid(v: ValidationOp[UUID]*): ALG[UUID] =
    javaUtilInject(UuidData(v.toList))

  /** Alias for UUID without validations */
  val uuid: ALG[UUID] = uuid()
}
