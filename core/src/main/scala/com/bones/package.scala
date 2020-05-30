package com

import com.bones.data.custom.AllCustomSyntax
import com.bones.data.{HasManifest, KeyValueDefinitionSugar, ListData, OptionalKvpValueDefinition, Sugar}
import com.bones.validation.ValidationDefinition._

/**
  * Collect all functionality here so one only needs to specify one import statement: 'com.bones.syntax._'
  */
package object bones {

  type Path = List[String]

  implicit class ToCollection[ALG[_], A: Manifest](hm: ALG[A]) { self =>
//    implicit val man = manifest[A]
    def list(validationOps: ValidationOp[List[A]]*) =
      ListData[ALG, A](Right(hm), validationOps.toList)

    def optional =
      OptionalKvpValueDefinition[ALG,A](Right(hm))
  }

  /** So we can just import com.bones.syntax._ */
  object syntax extends Sugar with AllCustomSyntax {

    /** This type is useful when we are not dealing with a custom algebra. */
    type NoAlgebra[A] = Nothing

  }

}
