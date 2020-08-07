package com.bones.interpreter

import com.bones.data.{KvpCoNil, KvpCoproduct, KvpSingleValueLeft}
import com.bones.data._
import shapeless.{Coproduct, HList, Nat}

/**
  * Just a template to be used as a starting point for a new interpreter.
  * You can copy/paste this for a starting point for a new interpreter.
  */
class InterpreterTemplate {

  def kvpCollection[ALG[_], H <: HList, HL <: Nat](group: KvpCollection[ALG, H, HL]): Unit = {
    group match {
      case nil: KvpNil[_]                                  => ???
      case op: KvpSingleValueHead[alg, h, t, tl, a]        => ???
      case op: KvpCollectionHead[alg, a, al, h, hl, t, tl] => ???
      case op: KvpConcreteValueHead[alg, a, ht, nt]        => ???
    }
  }

  def kvpCoproduct[ALG[_], C <: Coproduct](co: KvpCoproduct[ALG, C]): Unit = {
    co match {
      case nil: KvpCoNil[_]                  => ???
      case co: KvpSingleValueLeft[alg, l, r] => ???
    }
  }

  def determineValueDefinition[ALG[_], A](
    value: Either[ConcreteValue[ALG, A], ALG[A]],
    interpreter: Nothing): Unit = ???

  def valueDefinition[ALG[_], A](fgo: ConcreteValue[ALG, A]): Unit =
    fgo match {
      case op: OptionalValue[alg, a]        => ???
      case ld: ListData[alg, t]             => ???
      case ed: EitherData[alg, a, b]        => ???
      case kvp: KvpHListValue[alg, h, hl]   => ???
      case co: CoproductCollection[alg, c]  => ???
      case x: SwitchEncoding[alg, a, al, b] => ???
      case co: CoproductSwitch[alg, c, a]   => ???
    }

}
