package com

import com.bones.data.algebra.AllAlgebrasSyntax

/**
  * Collect all functionality here so one only needs to specify one import statement: 'com.bones.syntax._'
  */
package object bones {

  type Path = List[String]

  /** So we can just import com.bones.syntax._ */
  val syntax = AllAlgebrasSyntax

}
