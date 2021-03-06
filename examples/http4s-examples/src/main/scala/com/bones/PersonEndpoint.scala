package com.bones

import cats.effect.IO
import com.bones.fullstack.LocalhostAllIOApp
import com.bones.fullstack.LocalhostAllIOApp._
import com.bones.syntax._
import com.bones.validation.ValidationDefinition.{IntValidation => iv, StringValidation => sv}
import com.zaxxer.hikari.HikariDataSource
import org.http4s.HttpRoutes

/** Example endpoint.  This creates a complete application which saves a person to a local database including:
  * JSON endpoints, Protobuf Endpoints, 5 CRUD Endpoints (Get, Put, Post, Delete, Search AllCustomAlgebras),
  * Swagger, DB DDL.
  */
object PersonEndpoint extends LocalhostAllIOApp {

  val ds: HikariDataSource = localhostDataSource

  case class Person(name: String, age: Int, gender: Option[String])

  val personSchema = (
    (
      "name",
      string(sv.matchesRegex("^[a-zA-Z ]*$".r)),
      "The name of the person must be alphanumeric",
      "John Doe") ::
      ("age", int(iv.min(0)), "The Age, In years, of the person.", 21) ::
      ("gender", string.optional) :<:
      kvpNil
  ).convert[Person]

  val idSchema = (("id", long(lv.positive)) :: kvpNil).encodedHead[Long]()

  override def services: HttpRoutes[IO] =
    serviceRoutesWithCrudMiddleware(
      com.bones.http4s.config.defaultLong,
      "person",
      personSchema,
      idSchema,
      parseIdF,
      com.bones.jdbc.select.defaultSelectInterpreter,
      com.bones.jdbc.dbSearchInterpreter,
      com.bones.jdbc.insert.defaultDbInsertInterpreter,
      com.bones.jdbc.update.defaultDbUpdate,
      com.bones.jdbc.dbDeleteInterpreter,
      ds
    )
}
