package com.bones.http4s

import java.nio.charset.StandardCharsets

import cats.effect._
import cats.syntax.all._
import com.bones.data.KvpCollection.headTypeName
import com.bones.data.{KvpCollection, KvpNil}
import com.bones.http4s.BaseCrudInterpreter.StringToIdError
import com.bones.http4s.config.InterpreterConfig
import com.bones.swagger.{CrudOasInterpreter, SwaggerCoreInterpreter}
import fs2.Stream
import io.swagger.v3.oas.models.OpenAPI
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.dsl.impl.Path

object ClassicCrudInterpreter {

  /** Creates a CRUD definition, but no user defined functions, so no actual endpoints.
    * To be used with the [ClassicCrudInterpreter.withCreate], [ClassicCrudInterpreter.withRead],
    *   [ClassicCrudInterpreter.withUpdate], [ClassicCrudInterpreter.withDelete] and [ClassicCrudInterpreter.withSearch].  Use
    *   this method if not all CRUD endpoints should be implemented.
    *   Use the [allVerbs] method if all CRUD endpoints should be implemented.
    *   See [ClassicCrudInterpreter] for details on the parameters.
    */
  def empty[ALG[_], A: Manifest, E, F[_], ID: Manifest](
    path: String,
    interpreters: InterpreterConfig[ALG, ID],
    schema: KvpCollection[ALG, A],
    pathStringToId: String => Either[StringToIdError, ID],
    errorSchema: KvpCollection[ALG, E]
  )(
    implicit F: Sync[F],
    H: Http4sDsl[F]
  ): ClassicCrudInterpreter[ALG, A, E, F, ID] =
    ClassicCrudInterpreter(
      interpreters,
      path,
      schema,
      pathStringToId,
      errorSchema,
      None,
      None,
      None,
      None,
      None
    )

  /**
    * Creates a CRUD definition so that all CRUD verbs (GET, POST, PUT, DELETE) are supported.  If only a sub-set of Verbs
    * are desired, see [emptyCoreAlgebra].
    *   See [ClassicCrudInterpreter] for details on the parameters.
    */
  def allVerbs[ALG[_], A: Manifest, E, F[_], ID: Manifest](
    interpreters: InterpreterConfig[ALG, ID],
    path: String,
    schema: KvpCollection[ALG, A],
    pathStringToId: String => Either[StringToIdError, ID],
    errorSchema: KvpCollection[ALG, E],
    createF: A => F[Either[E, ID]],
    readF: ID => F[Either[E, (ID, A)]],
    updateF: (ID, A) => F[Either[E, ID]],
    deleteF: ID => F[Either[E, (ID, A)]],
    searchF: () => Stream[F, (ID, A)]
  )(
    implicit F: Sync[F],
    H: Http4sDsl[F]
  ): ClassicCrudInterpreter[ALG, A, E, F, ID] = ClassicCrudInterpreter(
    interpreters,
    path,
    schema,
    pathStringToId,
    errorSchema,
    Some(createF),
    Some(readF),
    Some(updateF),
    Some(deleteF),
    Some(searchF)
  )
}

/**
  * Builds out CREATE, READ, UPDATE and DELETE endpoints given a bones Schema and some other metadata.
  * @param path The http path to this API
  * @param schema The bones schema describing the data that is available at this REST endpoint.
  * @param pathStringToId Need to be able to convert the path/id from a String to the ID type.  For
  *                       instance, url path GET /object/1 where 1 is the ID to convert.  This comes in as a string.
  * @param errorSchema Used to describe how we want to display System Error data.
  * @param createF User defined function which is called after validating the input value.
  *                Probably used to create the data in data store.
  *                If None, no create endpoint will be created.
  * @param readF User defined function which is called to look up an entity in a data store by id.
  *              if None, no read endpoint will be created
  * @param updateF User defined function which is called after validating the input value to update the data in a data store.
  *                If None, no update endpoint will be created.
  * @param deleteF User defined function to delete data in a data store.
  *                If None, no delete endpoint will be created.
  * @param searchF user defined function to return all entities in a data store.
  *                If None, no search (get all) endpoint will be created.
  *                This needs to be improved to provide search by parameters.
  * @param F Should be an implementation of Sync, IO is a good default choice.
  * @tparam A The class Value of the endpoint defined by the bones Schema.
  * @tparam E The error type of user defined functions.
  * @tparam F An subclass of the Sync type class
  * @tparam ID The ID type.
  */
case class ClassicCrudInterpreter[ALG[_], A: Manifest, E, F[_], ID: Manifest](
  interpreters: InterpreterConfig[ALG, ID],
  path: String,
  schema: KvpCollection[ALG, A],
  pathStringToId: String => Either[StringToIdError, ID],
  errorSchema: KvpCollection[ALG, E],
  createF: Option[A => F[Either[E, ID]]] = None,
  readF: Option[ID => F[Either[E, (ID, A)]]] = None,
  updateF: Option[(ID, A) => F[Either[E, ID]]] = None,
  deleteF: Option[ID => F[Either[E, (ID, A)]]] = None,
  searchF: Option[() => Stream[F, (ID, A)]]
)(implicit F: Sync[F], H: Http4sDsl[F]) {

  /** Add or overwrite the existing user defined function to create. Adding a create function
    * will ensure the creation of a Create(PUT) endpoint.
    * @param create The function which is to be called during Create input.
    * @return Copy of ClassicCrudInterpreter with the new create function.
    */
  def withCreate(create: A => F[Either[E, ID]]): ClassicCrudInterpreter[ALG, A, E, F, ID] =
    this.copy(createF = Some(create))

  /** Add or overwrite the existing user defined function to find data in a data store.  Adding
    * a read function will ensure the create of a GET endpoint.
    * @param read The function to be called to look up an entity in a data store.
    * @return Copy of the ClassicCrudInterpreter with the new create function
    */
  def withRead(read: ID => F[Either[E, (ID, A)]]): ClassicCrudInterpreter[ALG, A, E, F, ID] =
    this.copy(readF = Some(read))

  /** Add or overwrite the existing user defined function to update data in a data store.  Adding
    * a update function will ensure the create of a POST endpoint.
    * @param update The function to be called to update an entity in a data store.
    * @return Copy of the ClassicCrudInterpreter with the new update function
    */
  def withUpdate(update: (ID, A) => F[Either[E, ID]]): ClassicCrudInterpreter[ALG, A, E, F, ID] =
    this.copy(updateF = Some(update))

  /** Add or overwrite the existing user defined function to delete data in a data store.  Adding
    * a delete function will ensure the create of a POST endpoint.
    * @param delete The function to be called to delete an entity in a data store.
    * @return Copy of the ClassicCrudInterpreter with the new delete function
    */
  def withDelete(delete: ID => F[Either[E, (ID, A)]]): ClassicCrudInterpreter[ALG, A, E, F, ID] =
    this.copy(deleteF = Some(delete))

  /** Add or overwrite the existing user defined function to search for data in a data store.  Adding
    * a search function will ensure the create of a GET endpoint to return all entities.
    * @param search The function to be called to search for all entities in a data store.
    * @return Copy of the ClassicCrudInterpreter with the new delete function
    */
  def withSearch(search: () => Stream[F, (ID, A)]): ClassicCrudInterpreter[ALG, A, E, F, ID] =
    this.copy(searchF = Some(search))

  val schemaWithId: KvpCollection[ALG, (ID, A)] = {
    (("id", interpreters.idDefinition) :: schema :: new KvpNil[ALG]).tupled[(ID, A)]
  }

  val idSchema: KvpCollection[ALG, ID] = {
    (("id", interpreters.idDefinition) :: new KvpNil[ALG]).encodedHead()
  }

  def createRoutes: HttpRoutes[F] = {

    object http extends BaseCrudInterpreter[ALG, A, E, (ID, A), F, ID]

    val updateHttpService =
      updateF.toList.flatMap(update => {
        BaseCrudInterpreter.updateRoute(
          path,
          pathStringToId,
          update,
          schema,
          errorSchema,
          idSchema,
          interpreters.jsonValidator,
          interpreters.jsonEncoder,
          interpreters.bsonValidator,
          interpreters.bsonEncoder,
          interpreters.protobufValidator,
          interpreters.protobufEncoder,
          interpreters.charset
        )
      })

    val readHttpService = readF.toList.flatMap(read => {
      BaseCrudInterpreter.httpGetRoute(
        path,
        pathStringToId,
        read,
        errorSchema,
        schemaWithId,
        interpreters.jsonEncoder,
        interpreters.bsonEncoder,
        interpreters.protobufEncoder,
        interpreters.charset
      )
    })

    val searchHttpService = searchF.toList.flatMap(search => {
      BaseCrudInterpreter.httpSearch(
        path,
        search,
        schemaWithId,
        interpreters.jsonEncoder,
        interpreters.charset
      )
    })

    val createHttpService = createF.toList.flatMap(create => {
      BaseCrudInterpreter.httpPostRoutes(
        path,
        create,
        schema,
        errorSchema,
        idSchema,
        interpreters.jsonValidator,
        interpreters.jsonEncoder,
        interpreters.bsonValidator,
        interpreters.bsonEncoder,
        interpreters.protobufValidator,
        interpreters.protobufEncoder,
        interpreters.charset
      )
    })

    val deleteHttpService = deleteF.toList.flatMap(del => {
      BaseCrudInterpreter.httpDeleteRoutes(
        path,
        pathStringToId,
        del,
        errorSchema,
        schemaWithId,
        interpreters.jsonEncoder,
        interpreters.bsonEncoder,
        interpreters.protobufEncoder,
        interpreters.charset
      )
    })

    val contentTypes = "application/json" :: "application/ubjson" :: "application/protobuf" :: Nil
    val swaggerHtml = swaggerDoc(
      contentTypes,
      interpreters.customSwaggerInterpreter,
      schema,
      schemaWithId,
      errorSchema,
      path
    )

    val swagger = http.htmlEndpoint(Path(s"swagger/$path"), swaggerHtml)

    val services: List[HttpRoutes[F]] =
      http.protoBuff(path, interpreters.protobufFile, schema, schemaWithId, errorSchema) :: swagger :: createHttpService ::: readHttpService ::: updateHttpService ::: deleteHttpService ::: searchHttpService

    services.foldLeft[HttpRoutes[F]](HttpRoutes.empty)(
      (op1: HttpRoutes[F], op2: HttpRoutes[F]) => op1 <+> op2
    )

  }

  /** Create an endpoint to display the swagger doc for classic Crud this type. */
  def swaggerDoc(
    contentTypes: List[String],
    customInterpreter: SwaggerCoreInterpreter[ALG],
    schema: KvpCollection[ALG, A],
    schemaWithId: KvpCollection[ALG, (ID, A)],
    errorSchema: KvpCollection[ALG, E],
    path: String
  ): String = {

    val openApi =
      CrudOasInterpreter.jsonApiForService(
        path,
        "Rest Service",
        "1.0",
        contentTypes,
        schema,
        schemaWithId,
        errorSchema,
        customInterpreter,
        createF.isDefined,
        readF.isDefined,
        updateF.isDefined,
        deleteF.isDefined,
        searchF.isDefined
      )(new OpenAPI())
    io.swagger.v3.core.util.Json.mapper().writeValueAsString(openApi)

  }

}
