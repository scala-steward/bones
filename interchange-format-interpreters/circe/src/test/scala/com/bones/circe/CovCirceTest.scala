package com.bones.circe

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId}
import java.util.Locale

import com.bones.circe.custom.BaseScalaCoreEncoder
import com.bones.data.custom.ScalaCoreValue
import com.bones.interpreter.KvpInterchangeFormatEncoderInterpreter.InterchangeFormatEncoder
import com.bones.schemas.CustomCovSchema._
import io.circe.Json
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers
import org.scalatestplus.scalacheck.Checkers
import shapeless.{:+:, CNil, Inl, Inr}

class CovCirceTest extends AnyFunSuite with Checkers with Matchers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(minSuccessful = 1000, workers = 5)

  test("scalacheck allSupport types - marshall then unmarshall with custom algebra") {
    val validateFromCirce = IsoCirceEncoderAndValidatorInterpreter

    //  val jsonToCc = validateFromCirce.byteArrayFuncFromSchema(allSupportCaseClass, Charset.forName("UTF8"))

    val dateFormatter: DateTimeFormatter =
      DateTimeFormatter
        .ofPattern("uuuuMMddHHmmss.SX")
        .withLocale(Locale.FRANCE)
        .withZone(ZoneId.of("UTC"))

    def customAlgebraEncoder[A]: CustomAlgebra[A] => A => Json =
      alg =>
        alg match {
          case MarkdownData =>
            str =>
              Json.fromString(str.asInstanceOf[String])
      }

    def dateExtAlgebraEncoder[A]: DateExtAlgebra[A] => A => Json =
      alg =>
        alg match {
          case InstantData =>
            i =>
              Json.fromString(dateFormatter.format(i.asInstanceOf[Instant]))
      }

    object BlogEncoder extends InterchangeFormatEncoder[BlogAlgebra, Json] {

      def encode[A](alg: BlogAlgebra[A]): A => Json =
        alg match {
          case Inl(customAlgebra)            => customAlgebraEncoder(customAlgebra)
          case Inr(Inl(dateExtAlgebra))      => dateExtAlgebraEncoder(dateExtAlgebra)
          case Inr(Inr(Inl(scalaCoreValue))) => BaseScalaCoreEncoder.encode(scalaCoreValue)
          case Inr(Inr(Inr(_)))              => sys.error("Unreachable code")
        }
    }

    val blogPostToJson = IsoCirceEncoderAndValidatorInterpreter
      .encoderFromCustomSchema(BlogPost.blogPostSchema, BlogEncoder)

    val blogPost = BlogPost(1, "title", List("tag1", "tag2"), Instant.now(), "Here is some content")

    val json = blogPostToJson.apply(blogPost)
    val jsonString = json.spaces2

//    println(jsonString)

//    val newCc = jsonToCc(jsonString)
//    newCc match {
//      case Left(x) =>
//        fail(s"expected success, received $x for JSON string ${io.circe.parser.parse(new String(jsonString, utf8))}")
//      case Right(newCc2) =>
//        newCc2.fancyEquals(cc)
//    }

  }

}
