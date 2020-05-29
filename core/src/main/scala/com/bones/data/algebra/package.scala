package com.bones.data

import shapeless.ops.coproduct
import shapeless.{:+:, CNil, Inl, Inr}

package object algebra {

  type CNilF[A] = CNil // trick to make things consistent on kind-level

  /* This Type ties all the custom algebras together into a single coproduct */
  type CustomStringCoproduct[A] = CustomStringValue[A] :+: CNilF[A]
  type AllAlgebras[A] = ScalaCoreValue[A] :+: ScalaContainerValue[A] :+: JavaUtilValue[A] :+: JavaTimeValue[A] :+: CustomStringCoproduct[A]

  /** This is to allow smart constructors for each custom algebra, so that the data structure is lifted into the
    * context of AllCustomAlgebras type.  For example, the smart constructor `email` for creating an email data type would
    *  become Inr(Inl(EmailData()) which satisfies the AllCustomAlgebras definition.
    * */
  object AllAlgebrasSyntax
      extends ScalaCoreValueSugarInjected[AllAlgebras]
      with ScalaContainerValueSugarInjected[AllAlgebras]
      with JavaUtilValueSugarInjected[AllAlgebras]
      with JavaTimeValueSugarInjected[AllAlgebras]
      with CustomStringValueSugarInjected[AllAlgebras] {


    /** Defines the context (the coproduct) of which we are Injecting definitions of this algebra into. */
    override def scalaCoreInject[A]: coproduct.Inject[AllAlgebras[A], ScalaCoreValue[A]] =
      Inl(_)

    /** Defines the context (the coproduct) of which we are Injecting definitions of this algebra into. */
    override def scalaContainerInject[A]: coproduct.Inject[AllAlgebras[A], ScalaContainerValue[A]] =
      i => Inr(Inl(i))

    /** Defines the context (the coproduct) of which we are Injecting definitions of this algebra into. */
    override def javaUtilInject[A]: coproduct.Inject[AllAlgebras[A], JavaUtilValue[A]] =
      i => Inr(Inr(Inl(i)))

    override def javaTimeInject[A]: coproduct.Inject[AllAlgebras[A], JavaTimeValue[A]] =
      i => Inr(Inr(Inr(Inl(i))))

    override def stringValueInject: coproduct.Inject[AllAlgebras[String], CustomStringValue[String]] =
      i => Inr(Inr(Inr(Inr(Inl(i)))))
  }

}
