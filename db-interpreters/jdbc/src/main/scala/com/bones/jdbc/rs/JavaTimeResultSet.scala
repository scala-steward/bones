package com.bones.jdbc.rs

import java.sql.ResultSet
import java.time.{LocalDateTime, ZoneId}
import java.util.Date

import cats.data.NonEmptyList
import com.bones.data.Error
import com.bones.data.values.{JavaTimeValue, LocalDateData, LocalDateTimeData}
import com.bones.jdbc.FindInterpreter.{FieldName, Path, utcCalendar}

trait JavaTimeResultSet extends ResultSetValue[JavaTimeValue] {
  override def resultSet[A](alg: JavaTimeValue[A])
    : (Path, FieldName) => ResultSet => Either[NonEmptyList[Error.ExtractionError], A] =
    alg match {
      case ld: LocalDateData =>
        (path, fieldName) => rs =>
          catchSql(rs.getDate(fieldName, utcCalendar), ld.typeName, path, ld)
            .map(date => date.toLocalDate)
      case dd: LocalDateTimeData =>
        (path, fieldName) => rs =>
          catchSql(rs.getDate(fieldName, utcCalendar), dd.typeName, path, dd)
            .map(date =>
              LocalDateTime.ofInstant(new Date(date.getTime).toInstant, ZoneId.of("UTC")))
      case _ => ??? // TODO

    }
}
