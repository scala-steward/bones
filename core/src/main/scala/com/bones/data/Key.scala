package com.bones.data

import com.bones.data.algebra.KvpHListValue
import shapeless.{Coproduct, HList, Nat}

/** A String key and it's value description where A is the type the value.
  *
  * @param key This is the sting token defining the Value.
  * @param dataDefinition This is the GADT representing a value type, can be either from the core Algebra or a custom Algebra.
  * @tparam A   This is the type which is "wrapped" by the GADT.
  * @tparam ALG Defines what algebra(s) we can use in a context.
  *             It can be [[com.bones.syntax.NoAlgebra]] (aka Nothing -- only core algebra)
  *             It can be [[com.bones.data.algebra.AllAlgebrasSyntax]]  (aka everything supported in Bones).
  *             It can be a single custom algebra such as [[com.bones.data.algebra.JavaTimeValue]]
  *             It can be any [[shapeless.Coproduct]] of Algebras.
  */
case class KeyValueDefinition[ALG[_], A](
  key: String,
  dataDefinition: ALG[A],
  description: Option[String],
  example: Option[A]
)

/** Useful DSL builder */
//trait KeyValueDefinitionSugar {
//
//  def kvp[ALG[_], A](key: String, valueDefinitionOp: ALG[A]): KeyValueDefinition[ALG, A] =
//    KeyValueDefinition[ALG, A](key, valueDefinitionOp, None, None)
//
//  def kvpCov[ALG[_], A](key: String, valueDefinitionOp: ALG[A]): KeyValueDefinition[ALG, A] =
//    KeyValueDefinition[ALG, A](key, valueDefinitionOp, None, None)
//
//  def kvpHList[ALG[_], H <: HList: Manifest, HL <: Nat](
//    key: String,
//    kvpHList: KvpHList[ALG, H, HL]
//  ): KeyValueDefinition[ALG, H] =
//    KeyValueDefinition[ALG, H](key, KvpHListValue(kvpHList, List.empty), None, None)
//
//  def kvpCoproduct[ALG[_], C <: Coproduct: Manifest](
//    key: String,
//    kvpCoproduct: ALG[C]
//  ): KeyValueDefinition[ALG, C] =
//    KeyValueDefinition[ALG, C](key, kvpCoproduct, None, None)
//
//}
