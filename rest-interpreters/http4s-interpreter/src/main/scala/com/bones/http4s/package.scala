package com.bones

import org.http4s.Request
import org.http4s.util.CaseInsensitiveString

package object http4s {

  /** Get content type from the Request Headers if it exists */
  def findContentType[F[_]](req: Request[F]): Option[String] =
    req.headers
      .find(header => header.name == CaseInsensitiveString("Content-Type"))
      .map(_.value)

  // TODO: Find a home for this function
  /** Create an endpoint to display the protobuf schema for each endpoint */
//  def protoBuff(
//                 path: String,
//                 customProtobufInterpreter: ProtoFileGeneratorInterpreter[ALG],
//                 schema: KvpCollection[String, ALG, A],
//                 schemaWithId: KvpCollection[String, ALG, (ID, A)],
//                 errorSchema: KvpCollection[String, ALG, E]
//               )(implicit F: Sync[F]): HttpRoutes[F] = {
//    def toFile[B] =
//      customProtobufInterpreter
//        .fromSchemaToProtoFile(_: KvpCollection[String, ALG, B])
//
//    val text =
//      s"""
//         | // Base Schema, Used for input on Create Only
//         | ${toFile(schema)}
//         |
//         | // Base Schema with ID, Used for other CRUD operations besides input on Create
//         | ${toFile(schemaWithId)}
//         |
//         | // Error Output Message
//         | ${toFile(errorSchema)}
//         |
//        """.stripMargin
//
//    HttpRoutes.of[F] {
//      case GET -> Root / "proto" / path =>
//        Ok(text, Header("Content-Type", "text/plain"))
//    }
//  }

}