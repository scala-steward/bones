package com.bones.bson.values

import cats.data.NonEmptyList
import com.bones.bson.BsonPrimitiveValidator
import com.bones.data.Error
import com.bones.data.Error.RequiredValue
import com.bones.data.values.CustomStringValue
import com.bones.interpreter.InterchangeFormatValidatorValue
import reactivemongo.bson.BSONValue

trait BsonCustomStringValidator
    extends InterchangeFormatValidatorValue[CustomStringValue, BSONValue] {

  override def validate[A](alg: CustomStringValue[A])
    : (Option[BSONValue], List[String]) => Either[NonEmptyList[Error.ExtractionError], A] =
    (bson, path) =>
      bson match {
        case Some(bsonVal) =>
          BsonPrimitiveValidator
            .extractString(Right(alg), "String")(bsonVal, path)
            .asInstanceOf[Either[NonEmptyList[Error.ExtractionError], A]]
        case None => Left(NonEmptyList.one(RequiredValue(path, alg.typeName)))

    }
}
