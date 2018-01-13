package com.ot.bones.compiler

import cats.Applicative
import com.ot.bones.transform.{OptionalTransform, Transform}
import com.ot.bones.validation.CustomConversionFromString.RequiredCustomExtraction
import com.ot.bones.validation.DateValidation.OptionalDateExtraction
import com.ot.bones.validation.IntValidation.RequiredInt
import com.ot.bones.validation.ToHList.{HList2, HList3, ToHListBonesOp, ToOptionalHListBonesOp}
import com.ot.bones.validation.StringValidation.{OptionalString, RequiredString}
import com.ot.bones.validation.UuidValidation.RequiredUuidExtraction
import com.ot.bones.{BonesOp, Key, RootKey, StringKey}

object DocCompiler {

  object Doc {
    implicit val docApp = new Applicative[Doc] {
      override def pure[A](x: A) = Doc("")
      override def ap[A, B](ff: Doc[A => B])(fa: Doc[A]): Doc[B] =
        Doc[B](s" ${ff.str} { ${fa.str} }")
    }
  }


  case class Doc[A](str: String)

  val docCompiler = new cats.arrow.FunctionK[BonesOp, Doc] {
    def keyDesc(key: Key) = key match {
      case StringKey(name) => s"with key ${name}"
      case RootKey => ""
    }

    def apply[A](fgo: BonesOp[A]): Doc[A] =
      fgo match {
        case key: Key => {
          Doc("")
        }
        case op: ToHListBonesOp[a] => {
          val members = op.members
          Doc(s"object with ${members.length} members: " + members.map(apply(_)).mkString("(", ")(", ")"))
        }
        case op: ToOptionalHListBonesOp[a] => {
          val members = op.members
          Doc(s"optional object with ${members.length} members: " + members.map(apply(_)).mkString("(", ")(", ")"))
        }
        case op: Transform[z,a] => {
          val r = apply(op.op)
          Doc(s"${r} mapped into class ${op.manifestA.runtimeClass.getSimpleName}")
        }
        case op: RequiredString => Doc(s"Required String ${keyDesc(op.key)}")
        case op: OptionalString  => Doc(s"Optional String ${keyDesc(op.key)}")
        case op: ToHListBonesOp[_] => Doc(s"Required object ${keyDesc(op.key)}.")
        case op: ToOptionalHListBonesOp[_] => Doc(s"Optional object ${keyDesc(op.key)} ")
        case op: RequiredInt => Doc(s"Required Int ${keyDesc(op.key)}")
        case op: OptionalDateExtraction => Doc(s"Required Date with format ${op.dateFormat})}")
        case op: OptionalTransform[a,b] => Doc(s"converted to Class ${op.manifestA.runtimeClass.getSimpleName}")
        case op: Transform[A,a] => Doc(s"converted to Class ${op.manifestA.runtimeClass.getSimpleName}")
        case op: RequiredUuidExtraction => Doc(s"Converted to UUID)}")
        case op: RequiredCustomExtraction[a] => Doc(s"Custom Conversion: ${op.description}")


        case _ => ???
      }
  }
}