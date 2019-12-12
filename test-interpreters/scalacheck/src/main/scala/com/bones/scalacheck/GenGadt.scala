package com.bones.scalacheck

import com.bones.data.{KeyValueDefinition, _}
import com.bones.validation.ValidationDefinition.StringValidation.{MaxLength, Trimmed}
import com.bones.validation.ValidationDefinition.ValidationOp
import org.scalacheck.Gen
import shapeless.ops.hlist.IsHCons
import shapeless.{::, HList, Nat, Succ}

object GenGadt {


  def genMaxStringLength: Gen[MaxLength] = for {
    len <- Gen.choose(0,5000)
  } yield MaxLength(len)

  def genTrimmed: Gen[Trimmed.type] = Gen.const(Trimmed)

  def genStringValidation: Gen[ValidationOp[String]] =
    Gen.oneOf(genMaxStringLength, genTrimmed)

  def genStringData: Gen[StringData] = for {
    validation <- genStringValidation
  } yield StringData(List(validation))

  def genDoubleData: Gen[DoubleData] = Gen.const(DoubleData(List.empty))

  def genKeys = Gen.oneOf(Scalacheck.loremIpsumWords)

  val types: Gen[String] = Gen.oneOf("String", "Double", "Object")
  val false90Percent: Gen[Boolean] = Gen.frequency((1,true), (9,false))

  def genHListValue[ALG[_]](): Gen[KvpHListValue[ALG, _<:HList, _<:Nat]] = for {
    strHead <- genStringData
    t <- types
    hList <- nextGen(false, "first", strHead, KvpNil[ALG](), t)
  } yield KvpHListValue(hList, List.empty)

  def nextGen[ALG[_], A,H<:HList,N<:Nat]
    (
      done: Boolean,
      key: String,
      newHead: KvpValue[A],
      tail: KvpHList[ALG, H,N], nextType: String
    ): Gen[KvpHList[ALG,_<:HList,_<:Nat]] = {

    val isHCons = implicitly[IsHCons.Aux[A :: H, A, H]]
    val thisValue: KvpHList[ALG, A::H,Succ[N]] = KvpSingleValueHead(KeyValueDefinition[ALG, A](key, Left(newHead)), List.empty, tail, isHCons)
    if (done) {
      Gen.const(thisValue)
    } else {
      nextType match {
        case "String" => genStringData.flatMap(sd => {
          genKvpSingleValueHead(sd, thisValue)
        })
        case "Double" => genDoubleData.flatMap(dd => {
          genKvpSingleValueHead(dd, thisValue)
        })
        case "Object" => {
          genKvpSingleValueHead(StringData(List.empty), KvpNil[ALG]()).flatMap(kvpHList => {
            val next = KvpHListValue(kvpHList, List.empty)
            genKeys.map(objKey => {
              KvpSingleValueHead(KeyValueDefinition(key, Left(next)), List.empty, thisValue, null)
            })
          })
        }
      }
    }
  }

  def genKvpSingleValueHead[ALG[_],A, H<:HList,N<:Nat]
    (
      newHead: KvpValue[A],
      tail: KvpHList[ALG,H,N]
    ): Gen[KvpHList[ALG,_<:HList, _<:Nat]] = {
      for {
        key <- genKeys
        t <- types
        done <- false90Percent
        next <- nextGen(done, key, newHead, tail, t)
      } yield {
        next
      }
    }
}
