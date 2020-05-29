package com.bones.data

import com.bones.data.HasManifest.AlgebraWithManifest

/**
  * If a custom algebra mixin in this trait, it's subtypes can mix in AlgToCollectionData.
 *
  * @tparam A The type of the Wrapped Value
  */
trait HasManifest[A] {
  val manifestOfA: Manifest[A]
}
object HasManifest {
  type AlgebraWithManifest[ALG[_], A] = ALG[A] with HasManifest[A]

}

object BonesSchema {
  def toBonesSchema[ALG[_], A](alg: AlgebraWithManifest[ALG,A]): BonesSchema[ALG,A] =
    BonesSchema(alg)

  trait ToBonesSchema[ALG[_], A] { self: AlgebraWithManifest[ALG,A] =>
    def toBonesSchema = BonesSchema.toBonesSchema[ALG,A](self)
  }
}

case class BonesSchema[ALG[_], A](alg: AlgebraWithManifest[ALG,A]) {
  val manifestOfA: Manifest[A] = alg.manifestOfA
}
