package com.bones.protobuf.custom

import com.bones.data.algebra.CustomStringValue
import com.bones.protobuf.ProtoFileGeneratorInterpreter

object CustomStringProtoFileInterpreter
    extends ProtoFileGeneratorInterpreter.CustomInterpreter[CustomStringValue] {

  import ProtoFileGeneratorInterpreter._

  override def toMessageField[A](alg: CustomStringValue[A]): (Name, Int) => (
    ProtoFileGeneratorInterpreter.MessageField,
    Vector[ProtoFileGeneratorInterpreter.NestedType],
    Int) =
    (name, index) => stringMessageField(name, index)
}
