package com.bones.swagger

import com.bones.data.{KvpCollection, HigherOrderValue}
import com.bones.swagger.SwaggerCoreInterpreter.CustomSwaggerInterpreter
import com.bones.swagger._
import io.swagger.v3.oas.models._
import io.swagger.v3.oas.models.info.Info
import io.swagger.v3.oas.models.media._
import io.swagger.v3.oas.models.parameters.{Parameter, RequestBody}
import io.swagger.v3.oas.models.responses.{ApiResponse, ApiResponses}

import scala.jdk.CollectionConverters._

/**
  * Responsible for creating a full CRUD Swagger definition including paths and data.
  *
  */
object CrudOasInterpreter {

  def jsonApiForService[ALG[_], A, E, ID](
    path: String,
    title: String,
    version: String,
    contentTypes: List[String],
    schema: KvpCollection[ALG, A],
    schemaWithId: KvpCollection[ALG, (ID, A)],
    errorSchema: KvpCollection[ALG, E],
    customAlgebraInterpreter: SwaggerCoreInterpreter[ALG],
    withCreate: Boolean,
    withRead: Boolean,
    withUpdate: Boolean,
    withDelete: Boolean,
    withSearch: Boolean
  ): OpenAPI => OpenAPI = { openApi =>
    if (withCreate) {
      CrudOasInterpreter
        .post(
          (schema, path),
          (schemaWithId, path),
          (errorSchema, "error"),
          s"/${path}",
          contentTypes,
          customAlgebraInterpreter
        )
        .apply(openApi)
    }

    if (withRead) {
      CrudOasInterpreter
        .get((schemaWithId, path), s"/${path}", customAlgebraInterpreter)
        .apply(openApi)
    }

    if (withUpdate) {
      CrudOasInterpreter
        .put(
          (schemaWithId, path),
          (schemaWithId, path),
          (errorSchema, "error"),
          s"/${path}",
          contentTypes,
          customAlgebraInterpreter
        )
        .apply(openApi)
    }

    if (withDelete) {
      CrudOasInterpreter
        .delete((schemaWithId, path), s"/${path}", contentTypes, customAlgebraInterpreter)
        .apply(openApi)
    }

    val info = new Info()
    info.title(title)
    info.version(version)
    openApi.info(info)
  }

  private def upsertComponent[A](openApi: OpenAPI, entityName: String, schema: Schema[A]): Unit = {
    var components = openApi.getComponents
    if (components == null) {
      components = new Components()
      openApi.setComponents(components)
    }
    var schemas = components.getSchemas
    if (schemas == null) {
      schemas = new java.util.HashMap[String, Schema[_]]()
      components.setSchemas(schemas)
    }
    if (!components.getSchemas.containsKey(entityName)) {
      components.addSchemas(entityName, schema)
    }
  }

  private def upcertPath(openAPI: OpenAPI, path: String, f: PathItem => Unit) = {
    var paths = openAPI.getPaths
    if (paths == null) {
      paths = new Paths()
      openAPI.setPaths(paths)
    }
    var pathItem = paths.get(path)
    if (pathItem == null) {
      pathItem = new PathItem()
      paths.addPathItem(path, pathItem)
    }
    f(pathItem)

  }

  def get[ALG[_], A](
    outputSchema: (KvpCollection[ALG, A], String),
    urlPath: String,
    interpreter: SwaggerCoreInterpreter[ALG]
  ): OpenAPI => OpenAPI = { openAPI =>
    val outputEntityName = outputSchema._2

    val inputSchemas =
      interpreter.generateSchemas(outputSchema._1)(outputEntityName)

    inputSchemas.foreach { schemas =>
      upsertComponent(openAPI, schemas._1, schemas._2)
    }

    val apiResponse = new ApiResponse()
      .$ref(s"#/components/schemas/${outputEntityName}")
    val apiResponses = new ApiResponses()
      .addApiResponse("200", apiResponse)

    val paramSchema = new IntegerSchema()
    val param = new Parameter()
      .name("id")
      .in("path")
      .required(true)
      .description(s"id of the ${outputEntityName} to retrieve")
      .schema(paramSchema)

    val operation = new Operation()
      .responses(apiResponses)
      .parameters(java.util.Collections.singletonList(param))
      .tags(java.util.Collections.singletonList(outputEntityName))
      .summary(s"Find ${outputEntityName} by ID")
      .description(s"Returns ${outputEntityName} by id")
      .operationId(s"get${outputEntityName.capitalize}ById")

    upcertPath(openAPI, "/{id}", _.get(operation))

    openAPI
  }

  def delete[ALG[_], O](
    outputSchemaWithName: (KvpCollection[ALG, O], String),
    urlPath: String,
    contentTypes: List[String],
    swaggerInterpreter: SwaggerCoreInterpreter[ALG]
  ): OpenAPI => OpenAPI = { openAPI =>
    val outputEntityName = outputSchemaWithName._2
    val outputComponentSchema =
      swaggerInterpreter.generateSchemas(outputSchemaWithName._1)(outputEntityName)

    outputComponentSchema.foreach {
      case (name, schema) => upsertComponent(openAPI, name, schema)
    }

    val apiResponse = new ApiResponse()
      .$ref(s"#/components/schemas/${outputEntityName}")
    val apiResponses = new ApiResponses()
      .addApiResponse("200", apiResponse)

    val paramSchema = new IntegerSchema()
    val param = new Parameter()
      .name("id")
      .in("path")
      .required(true)
      .description(s"id of the ${outputEntityName} to delete")
      .schema(paramSchema)

    val operation = new Operation()
      .responses(apiResponses)
      .parameters(java.util.Collections.singletonList(param))
      .tags(java.util.Collections.singletonList(outputEntityName))
      .summary(s"Delete ${outputEntityName} by ID")
      .description(s"Delete ${outputEntityName} by id")
      .operationId(s"delete${outputEntityName.capitalize}ById")

    upcertPath(openAPI, "/{id}", _.delete(operation))
    openAPI
  }

  def put[ALG[_], I, O, E](
    inputSchemaAndName: (KvpCollection[ALG, I], String),
    outputSchemaAndName: (KvpCollection[ALG, O], String),
    errorSchemaAndName: (KvpCollection[ALG, E], String),
    urlPath: String,
    contentTypes: List[String],
    swaggerInterpreter: SwaggerCoreInterpreter[ALG]
  ): OpenAPI => OpenAPI = { openAPI =>
    val inputEntityName = inputSchemaAndName._2
    val outputEntityName = outputSchemaAndName._2
    val errorEntityName = errorSchemaAndName._2

    val inputComponentSchemas =
      swaggerInterpreter.generateSchemas(inputSchemaAndName._1)(inputEntityName)
    val outputComponentSchemas =
      swaggerInterpreter.generateSchemas(outputSchemaAndName._1)(outputEntityName)
    val errorComponentSchemas =
      swaggerInterpreter.generateSchemas(errorSchemaAndName._1)(errorEntityName)

    (inputComponentSchemas ::: outputComponentSchemas ::: errorComponentSchemas)
      .foreach { case (name, schema) => upsertComponent(openAPI, name, schema) }

    val outputComponentRef = s"#/components/schemas/${outputEntityName}"
    val inputComponentRef = s"#/components/schemas/${inputEntityName}"
    val errorComponentRef = s"#/components/schemas/${errorEntityName}"

    val apiResponses = new ApiResponses()
      .addApiResponse("200", new ApiResponse().$ref(outputComponentRef))
      .addApiResponse("400", new ApiResponse().$ref(errorComponentRef))

    val inputOasSchema = new Schema()
      .$ref(inputComponentRef)

    val paramSchema = new IntegerSchema()
    val param = new Parameter()
      .name("id")
      .in("path")
      .required(true)
      .description(s"id of the ${outputEntityName} to update")
      .schema(paramSchema)

    val operation = new Operation()
      .responses(apiResponses)
      .parameters(java.util.Collections.singletonList(param))
      .tags(java.util.Collections.singletonList(outputEntityName))
      .summary(s"Update ${inputEntityName}, returning ${outputEntityName}")
      .description(s"Update and return the updated ${outputEntityName}")
      .operationId(s"put${inputEntityName.capitalize}ById")

    val content = new Content()

    contentTypes.foreach(contentType => {
      val encoding = new Encoding().contentType(contentType)
      val encodings = Map((contentType, encoding))
      val mediaType = new MediaType()
        .schema(inputOasSchema)
        .encoding(encodings.asJava)

      content
        .addMediaType(contentType, mediaType)

      val requestBody = new RequestBody()
        .content(content)

      operation.setRequestBody(requestBody)
    })

    upcertPath(openAPI, "/{id}", _.put(operation))

    openAPI

  }

  def post[ALG[_], I, O, E](
    inputSchemaAndName: (KvpCollection[ALG, I], String),
    outputSchemaAndName: (KvpCollection[ALG, O], String),
    errorSchemaAndName: (KvpCollection[ALG, E], String),
    urlPath: String,
    contentTypes: List[String],
    interpreter: SwaggerCoreInterpreter[ALG]
  ): OpenAPI => OpenAPI = { openAPI =>
    val inputEntityName = inputSchemaAndName._2
    val outputEntityName = outputSchemaAndName._2
    val errorEntityName = errorSchemaAndName._2

    val inputComponentSchemas =
      interpreter.generateSchemas(inputSchemaAndName._1)(inputEntityName)
    val outputComponentSchemas =
      interpreter.generateSchemas(outputSchemaAndName._1)(outputEntityName)
    val errorComponentSchemas =
      interpreter.generateSchemas(errorSchemaAndName._1)(errorEntityName)

    (inputComponentSchemas ::: outputComponentSchemas ::: errorComponentSchemas)
      .foreach { case (name, schema) => upsertComponent(openAPI, name, schema) }

    val outputComponentRef = s"#/components/schemas/${outputEntityName}"
    val inputComponentRef = s"#/components/schemas/${inputEntityName}"
    val errorComponentRef = s"#/components/schemas/${errorEntityName}"

    val apiResponses = new ApiResponses()
      .addApiResponse("200", new ApiResponse().$ref(outputComponentRef))
      .addApiResponse("400", new ApiResponse().$ref(errorComponentRef))

    val inputOasSchema = new Schema()
      .$ref(inputComponentRef)

    val operation = new Operation()
      .responses(apiResponses)
      .tags(java.util.Collections.singletonList(outputEntityName))
      .summary(s"Create ${inputEntityName}, returning ${outputEntityName}")
      .description(s"Create and return the newly created ${outputEntityName}")
      .operationId(s"post${inputEntityName}")

    val content = new Content()

    contentTypes.foreach(contentType => {
      val encoding = new Encoding().contentType(contentType)
      val encodings = Map((contentType, encoding))
      val mediaType = new MediaType()
        .schema(inputOasSchema)
        .encoding(encodings.asJava)
      content
        .addMediaType(contentType, mediaType)

    })
    val requestBody = new RequestBody()
      .content(content)

    operation.setRequestBody(requestBody)

    upcertPath(openAPI, "/", _.post(operation))

    openAPI
  }

}
