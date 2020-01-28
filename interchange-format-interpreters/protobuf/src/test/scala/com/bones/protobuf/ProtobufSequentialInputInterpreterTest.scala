package com.bones.protobuf

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}
import java.util.{Base64, UUID}

import com.bones.syntax._
import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import org.scalatest.{FunSuite, MustMatchers}
import org.scalatestplus.scalacheck.Checkers

class ProtobufSequentialInputInterpreterTest extends FunSuite with Checkers with MustMatchers{

  case class Loc(city: String, state: String)
  case class Person(id: UUID, name: String, age: Long, location: Loc, knowsAboutGadt: Boolean, favoriteColor: Option[String])

  val monica = Person(UUID.randomUUID(), "Monica", 44l, Loc("Denver", "CO"), true, Some("black"))

  val loc = (
      ("city", string) ::
      ("state", string) ::
      kvpNil
  ).convert[Loc]

  val person = (
    ("id", uuid) ::
    ("name", string) ::
    ("age", long) ::
    ("location", loc) ::
    ("knowsAboutGadt", boolean) ::
    ("favoriteColor", string.optional) ::
    kvpNil
  ).convert[Person]

  ignore("single items") {

    val denver = Loc("Denver", "CO")
    val bytes = UtcProtobufSequentialOutputInterpreter.encodeToBytes(loc)(denver)

    val is = new ByteArrayInputStream(bytes)
    val cin: CodedInputStream = CodedInputStream.newInstance(is)

    val isItDenver = UtcProtobufSequentialInputInterpreter.fromBytes(loc)(bytes)

    isItDenver match {
      case Right(l) => l mustBe denver
      case Left(err) => fail(s"Expected loc, received: ${err}")
    }



  }

  ignore("Person") {

    val os = new ByteArrayOutputStream()
    val cos: CodedOutputStream = CodedOutputStream.newInstance(os)

    val result = ProtoFileInterpreter.fromSchema(person)

    val str = ProtoFileInterpreter.messageToProtoFile(result)

    val bytes = UtcProtobufSequentialOutputInterpreter.encodeToBytes(person)(monica)

    println("result:" + Base64.getEncoder.encodeToString(bytes))

    val isItMonica = UtcProtobufSequentialInputInterpreter.fromBytes(person)(bytes)

    isItMonica match {
      case Right(person) => person mustBe monica
      case Left(err) => fail(s"Expected monica, received: ${err}")
    }
  }

  def convertBytesToHex(bytes: Seq[Byte]): String = {
    val sb = new StringBuilder
    for (b <- bytes) {
      sb.append(String.format("%02x", Byte.box(b)))
    }
    sb.toString
  }

}
