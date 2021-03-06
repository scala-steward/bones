package com.bones.interpreter

import cats.data.NonEmptyList
import com.bones.data.Error.ExtractionError
import com.bones.data.values.CNilF
import shapeless.{:+:, Coproduct, Inl, Inr}

object InterchangeFormatValidatorValue {

  /** using kind projector allows us to create a new interpreter by merging two existing interpreters */
  def merge[L[_], R[_] <: Coproduct, A, OUT](
                                              li: InterchangeFormatValidatorValue[L, OUT],
                                              ri: InterchangeFormatValidatorValue[R, OUT]
  ): InterchangeFormatValidatorValue[Lambda[A => L[A] :+: R[A]], OUT] =
    new InterchangeFormatValidatorValue[Lambda[A => L[A] :+: R[A]], OUT] {
      override def validate[A](lr: L[A] :+: R[A])
        : (Option[OUT], List[String]) => Either[NonEmptyList[ExtractionError], A] = lr match {
        case Inl(l) => li.validate(l)
        case Inr(r) => ri.validate(r)
      }

    }

  implicit class InterpreterOps[ALG[_], OUT](val base: InterchangeFormatValidatorValue[ALG, OUT])
      extends AnyVal {
    def ++[R[_] <: Coproduct](r: InterchangeFormatValidatorValue[R, OUT])
      : InterchangeFormatValidatorValue[Lambda[A => ALG[A] :+: R[A]], OUT] =
      merge(base, r)

  }

  case class CNilInterchangeFormatValidator[OUT]() extends InterchangeFormatValidatorValue[CNilF, OUT] {
    override def validate[A](
      alg: CNilF[A]): (Option[OUT], List[String]) => Either[NonEmptyList[ExtractionError], A] =
      sys.error("Unreachable code")
  }
}

trait InterchangeFormatValidatorValue[ALG[_], IN] {
  def validate[A](
    alg: ALG[A]): (Option[IN], List[String]) => Either[NonEmptyList[ExtractionError], A]
}
