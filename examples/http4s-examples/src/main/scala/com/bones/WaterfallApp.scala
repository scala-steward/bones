package com.bones

import java.time.{LocalDateTime, ZonedDateTime}

import cats.effect.IO
import com.bones.crud.Algebra.ServiceOps
import com.bones.data.Value.KvpNil
import com.bones.fullstack.CrudDbDefinitions.DbError
import com.bones.fullstack.LocalhostAllIOApp
import com.bones.syntax._
import com.bones.validation.ValidationDefinition.{BigDecimalValidation => dv, LongValidation => lv, StringValidation => sv}
import org.http4s.HttpRoutes
import cats.effect._
import cats.implicits._


object WaterfallDefinitions {

  //  case class Error(error: String)
  val errorDef = (kvp("error", string) :: KvpNil).convert[DbError]


  case class ImperialMeasurement(feet: Long, inches: Long)

  case class Location(latitude: BigDecimal, longitude: BigDecimal)

  val imperialMeasurement = (
    kvp("feet", long(lv.min(0))) ::
      kvp("inches", long(lv.between(0, 12))) ::
      KvpNil
    ).convert[ImperialMeasurement]

  object WaterVolume extends Enumeration {
    type WaterVolume = Value
    val Low, Average, High = Value
  }


  case class Waterfall(name: String, latitude: BigDecimal, longitude: BigDecimal, cubicFeetPerMinute: Option[BigDecimal],
                       height: Option[ImperialMeasurement], waterValue: WaterVolume.Value, // discoveryDate: ZonedDateTime,
                       wantToVisit: Boolean)

  val waterfall = (
    kvp("name", string(sv.max(200))) ::
      kvp("latitude", bigDecimal(dv.min(-180), dv.max(180))) ::
      kvp("longitude", bigDecimal(dv.min(-180), dv.max(180))) ::
      kvp("cubicFeetPerMinute", bigDecimal(dv.positive).optional) ::
      kvp("height", imperialMeasurement.optional) ::
      kvp("waterVolume", enumeration[WaterVolume.Value](WaterVolume)) ::
      //      kvp("discoveryDate", isoDateTime()) ::
      kvp("wantToVisit", boolean) ::
      KvpNil
    ).convert[Waterfall]

  val waterfallService =
    ServiceOps.classicCrud("waterfall", waterfall, errorDef)


  case class WaterfallVisit(waterfallId: Long, waterVolume: WaterVolume.Value, notes: Option[String])

  val waterfallVisit = (
    kvp("waterfallId", long(lv.min(1))) ::
      //      kvp("visitDate", isoDate()) ::
      kvp("waterVolume", enumeration[WaterVolume.Value](WaterVolume)) ::
      kvp("notes", string.optional) ::
      KvpNil
    ).convert[WaterfallVisit]

  val waterfallVisitService =
    ServiceOps.havingPath("waterfallVisit")
      .providingCreate(waterfallVisit, waterfallVisit, errorDef)
      .providingRead(waterfallVisit, errorDef)
      .providingUpdate(waterfallVisit, waterfallVisit, errorDef)
      .providingDelete(waterfallVisit, errorDef)

}

object WaterfallApp extends LocalhostAllIOApp() {

  import LocalhostAllIOApp._
  import WaterfallDefinitions._

  val ds = localhostDataSource

  override def services: HttpRoutes[IO] = {
    serviceRoutesWithCrudMiddleware(waterfallService, ds) <+>
      //    serviceRoutesWithCrudMiddleware(waterfallVisitService, ds) <+>
      dbSchemaEndpoint(waterfallService) <+>
      dbSchemaEndpoint(waterfallVisitService) <+>
      reactEndpoints(List(waterfall, waterfallVisit))
  }
}
