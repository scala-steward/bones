package com.bones.protobuf

import com.bones.data.algebra.{AllAlgebras, CustomStringCoproduct}
import com.bones.protobuf.ProtoFileGeneratorInterpreter.CustomInterpreter
import com.bones.protobuf.ProtoFileGeneratorInterpreter.CustomInterpreter.CNilProtoFileCustomInterpreterEncoder
import com.bones.protobuf.ProtobufSequentialEncoderInterpreter.CustomEncoderInterpreter
import com.bones.protobuf.ProtobufSequentialEncoderInterpreter.CustomEncoderInterpreter.CNilCustomEncoder
import com.bones.protobuf.ProtobufSequentialValidatorInterpreter.CustomValidatorInterpreter
import com.bones.protobuf.ProtobufSequentialValidatorInterpreter.CustomValidatorInterpreter.CNilCustomValidatorEncoder

package object custom {

  def allEncoders[A]: CustomEncoderInterpreter[AllAlgebras] =
    ProtobufUtcJavaTimeEncoder ++
      (CustomStringEncoderInterpreter ++ CNilCustomEncoder: CustomEncoderInterpreter[
        CustomStringCoproduct])

  val allValidators: CustomValidatorInterpreter[AllAlgebras] =
    ProtobufUtcJavaTimeValidator ++
      (CustomStringValidatorInterpreter ++ CNilCustomValidatorEncoder: CustomValidatorInterpreter[
        CustomStringCoproduct])

  val allProtoFiles: ProtoFileGeneratorInterpreter.CustomInterpreter[AllAlgebras] =
    JavaTimeProtoFileInterpreter ++
      (CustomStringProtoFileInterpreter ++ CNilProtoFileCustomInterpreterEncoder: CustomInterpreter[
        CustomStringCoproduct])

  object ProtobufUtcJavaTimeEncoder extends JavaTimeEncoderEncoderInterpreter {
    override val coreProtobufSequentialOutputInterpreter: ProtobufSequentialEncoderInterpreter =
      ProtobufUtcSequentialEncoderAndValidator
  }

  object ProtobufUtcJavaTimeValidator extends JavaTimeValidatorInterpreter {
    override val coreProtobufSequentialInputInterpreter: ProtobufSequentialValidatorInterpreter =
      ProtobufUtcSequentialEncoderAndValidator
  }

}
