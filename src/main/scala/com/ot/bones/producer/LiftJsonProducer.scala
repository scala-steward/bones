package com.ot.bones.producer

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import com.ot.bones.data.Key
import com.ot.bones.interpreter.ExtractionInterpreter.{JsonProducer, WrongTypeError}
import net.liftweb.json.JsonAST._

case class LiftJsonProducer(jValue: JValue) extends JsonProducer {

  private def lookForString(jValue: JValue): Validated[WrongTypeError[String], Option[String]] = {
    jValue match {
      case JArray(_) => Invalid(WrongTypeError( classOf[String], classOf[JArray]))
      case JBool(_) => Invalid(WrongTypeError( classOf[String], classOf[JBool]))
      case JDouble(_) => Invalid(WrongTypeError( classOf[String], classOf[JDouble]))
//      case JField(_,JString(s)) => Valid(Some(s))
      case JInt(_) => Invalid(WrongTypeError( classOf[String], classOf[JInt]))
      case JNothing => Valid(None)
      case JObject(_) => Invalid(WrongTypeError( classOf[String], classOf[JObject]))
      case JString(str) => Valid(Some(str))
      case JNull =>  Valid(None)
    }
  }

  private def lookForDouble(jValue: JValue): Validated[WrongTypeError[Double], Option[Double]] = {
    jValue match {
      case JArray(_) => Invalid(WrongTypeError( classOf[Double], classOf[JArray]))
      case JBool(_) => Invalid(WrongTypeError( classOf[Double], classOf[JBool]))
      case JDouble(d) => Valid(Some(d))
//      case JField(_,_) => Invalid(WrongTypeError( classOf[Double], classOf[JField]))
      case JInt(_) => Invalid(WrongTypeError( classOf[Double], classOf[JInt]))
      case JNothing => Valid(None)
      case JObject(_) => Invalid(WrongTypeError( classOf[Double], classOf[JObject]))
      case JString(str) => Invalid(WrongTypeError( classOf[Double], classOf[JString]))
      case JNull =>  Valid(None)
    }
  }

  private def lookForInt(jValue: JValue): Validated[WrongTypeError[Int], Option[Int]] = {
    jValue match {
      case JArray(_) => Invalid(WrongTypeError( classOf[Int], classOf[Array[_]]))
      case JBool(_) => Invalid(WrongTypeError( classOf[Int], classOf[Boolean]))
      case JDouble(_) => Invalid(WrongTypeError( classOf[Int], classOf[Double]))
//      case JField(_,_) => Invalid(WrongTypeError( classOf[Int], classOf[Object]))
      case JInt(i) => Valid(Some(i.intValue()))
      case JNothing => Valid(None)
      case JObject(_) => Invalid(WrongTypeError( classOf[Int], classOf[Object]))
      case JString(_) => Invalid(WrongTypeError( classOf[Int], classOf[String]))
      case JNull =>  Valid(None)
    }
  }

  private def lookForObject(jValue: JValue): Validated[WrongTypeError[JsonProducer], Option[LiftJsonProducer]] = {
    jValue match {
      case JArray(_) => Invalid(WrongTypeError( classOf[JsonProducer], classOf[JArray]))
      case JBool(_) => Invalid(WrongTypeError( classOf[JsonProducer], classOf[JBool]))
      case JDouble(_) => Invalid(WrongTypeError( classOf[JsonProducer], classOf[JDouble]))
//      case JField(_,_) => Invalid(WrongTypeError( classOf[JsonProducer], classOf[JField]))
      case JInt(_) => Invalid(WrongTypeError( classOf[JsonProducer], classOf[JInt]))
      case JNothing => Valid(None)
      case JObject(obj) => Valid(Some(LiftJsonProducer(JObject(obj))))
      case JString(_) => Invalid(WrongTypeError( classOf[JsonProducer], classOf[JString]))
      case JNull =>  Valid(None)
    }
  }

  private def lookForBool(jValue: JValue): Validated[WrongTypeError[Boolean], Option[Boolean]] = {
    jValue match {
      case JArray(_) => Invalid(WrongTypeError( classOf[Boolean], classOf[Array[_]]))
      case JBool(b) => Valid(Some(b))
      case JDouble(_) => Invalid(WrongTypeError( classOf[Boolean], classOf[Double]))
//      case JField(_,_) => Invalid(WrongTypeError( classOf[Boolean], classOf[Object]))
      case JInt(_) => Invalid(WrongTypeError( classOf[Boolean], classOf[BigInt]))
      case JNothing => Valid(None)
      case JObject(_) => Invalid(WrongTypeError( classOf[Boolean], classOf[BigInt]))
      case JString(_) => Invalid(WrongTypeError( classOf[Boolean], classOf[String]))
      case JNull =>  Valid(None)
    }
  }

  private def lookForArray(jValue: JValue): Validated[WrongTypeError[List[_]], Option[List[LiftJsonProducer]]] = {
    jValue match {
      case a: JArray => Valid(Some(a.arr.map(child => LiftJsonProducer(child))))
      case JNothing => Valid(None)
      case JNull => Valid(None)
      case JDouble(_) => Invalid(WrongTypeError( classOf[List[_]], classOf[Double]))
//      case JField(_,_) => Invalid(WrongTypeError( classOf[List[_]], classOf[Object]))
      case JInt(_) => Invalid(WrongTypeError( classOf[List[_]], classOf[BigInt]))
      case JObject(_) => Invalid(WrongTypeError( classOf[List[_]], classOf[BigInt]))
      case JString(_) => Invalid(WrongTypeError( classOf[List[_]], classOf[String]))
      case JBool(_) => Invalid(WrongTypeError( classOf[List[_]], classOf[Boolean]))
    }
  }

  override def produceDouble: Validated[WrongTypeError[Double], Option[Double]] =
    lookForDouble(jValue)


  override def produceString: Validated[WrongTypeError[String], Option[String]] =
    lookForString(jValue)

  override def produceInt: Validated[WrongTypeError[Int], Option[Int]] = lookForInt(jValue)

  override def produceObject: Validated[WrongTypeError[JsonProducer], Option[LiftJsonProducer]] =
    lookForObject(jValue)

  override def produceBool: Validated[WrongTypeError[Boolean], Option[Boolean]] = lookForBool(jValue)

  override def produceList: Validated[WrongTypeError[List[_]], Option[List[JsonProducer]]] =
    lookForArray(jValue)

  override def child(key: Key): LiftJsonProducer = LiftJsonProducer( jValue \ key.name )
}
