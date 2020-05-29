package com.bones.scalacheck

import com.bones.data.algebra.{AllAlgebras, CustomStringCoproduct}
import com.bones.scalacheck.GenAlg.CNilGenEncoder

package object custom {

  val allInterpreters: GenAlg[AllAlgebras] =
    DefaultScalacheckJavaTimeInterpreter ++
      (DefaultCustomStringValueInterpreter ++ CNilGenEncoder: GenAlg[CustomStringCoproduct])

  object DefaultScalacheckJavaTimeInterpreter extends ScalacheckJavaTimeInterpreter
  object DefaultCustomStringValueInterpreter extends CustomStringValueInterpreter

}
