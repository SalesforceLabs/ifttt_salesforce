package utils

import akka.stream.scaladsl.Source
import akka.util.ByteString
import javax.inject.{Inject, Singleton}
import modules.Redis
import play.api.libs.Codecs
import play.api.libs.json._
import play.api.libs.ws.{WSClient, WSResponse}
import play.api.mvc.MultipartFormData.FilePart
import play.api.mvc.{Action, InjectedController, Result}
import play.api.{Configuration, Logging}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

@Singleton
class Force @Inject() (configuration: Configuration, ws: WSClient, redis: Redis) (implicit ec: ExecutionContext) extends InjectedController with Logging {

  import Force._

  lazy val salesforceOauthKey = configuration.get[String]("salesforce.oauth.key")
  lazy val salesforceOauthSecret = configuration.get[String]("salesforce.oauth.secret")

  lazy val managedPackageId = configuration.get[String]("salesforce.managed-package-id")

  private def bearerAuth(auth: String) = if (auth.startsWith("Bearer ")) auth else s"Bearer $auth"

  // todo: maybe put an in-memory cache here since this can get called a lot
  def userinfo(auth: String): Future[JsValue] = {
    redis.client.get[String](Codecs.sha1(bearerAuth(auth))).flatMap { maybeEnv =>
      val env = maybeEnv.getOrElse(ENV_PROD)
      ws.url(userinfoUrl(env)).withHttpHeaders(AUTHORIZATION -> bearerAuth(auth)).get().flatMap { userInfoResponse =>
        userInfoResponse.status match {
          case OK =>
            Future.successful(userInfoResponse.json)
          case UNAUTHORIZED | FORBIDDEN =>
            Future.failed(UnauthorizedException(userInfoResponse.body))
          case _ =>
            val jsonTry = Try(userInfoResponse.json)
            Future.failed(jsonTry.map(ForceError.apply).getOrElse(new Exception("Could not get user info: " + userInfoResponse.body)))
        }
      }
    }
  }

  def loginUrl(env: String) = env match {
    case ENV_PROD => "https://login.salesforce.com/services/oauth2/token"
    case ENV_SANDBOX => "https://test.salesforce.com/services/oauth2/token"
  }

  def userinfoUrl(env: String) = env match {
    case ENV_PROD => "https://login.salesforce.com/services/oauth2/userinfo"
    case ENV_SANDBOX => "https://test.salesforce.com/services/oauth2/userinfo"
  }

  private def on[T](status: Int)(body: WSResponse => T)(response: WSResponse): Future[T] = {
    response.status match {
      case `status` =>
        Future.successful(body(response))
      case BAD_REQUEST if (response.json \ "error").asOpt[String].contains("invalid_grant") =>
        val errorMessage = (response.json \ "error_description").as[String]
        Future.failed(UnauthorizedException(errorMessage))
      case UNAUTHORIZED =>
        val errorMessage = (response.json \\ "message").map(_.as[String]).mkString
        Future.failed(UnauthorizedException(errorMessage))
      case _ =>
        Future.failed(ForceError(response.json))
    }
  }

  def login(env: String, username: String, password: String): Future[JsValue] = {

    val body = Map(
      "grant_type" -> "password",
      "client_id" -> salesforceOauthKey,
      "client_secret" -> salesforceOauthSecret,
      "username" -> username,
      "password" -> password
    ).mapValues(Seq(_))

    ws.url(loginUrl(env)).post(body).flatMap {
      on(OK)(_.json)
    }
  }

  private def feedUrl(userInfo: JsValue, maybeCommunityId: Option[String]): String = {
    val path = maybeCommunityId.fold("chatter/feed-elements")(communityId => s"connect/communities/$communityId/chatter/feed-elements")
    restUrl(userInfo) + path
  }

  def chatterPostMessage(auth: String, message: String, maybeCommunityId: Option[String], groupId: Option[String]): Future[(JsValue, String)] = {
    userinfo(auth).flatMap { userInfo =>
      val userId = (userInfo \ "user_id").as[String]
      val subjectId = groupId.getOrElse(userId)

      val json = Json.obj(
        "body" -> Json.obj(
          "messageSegments" -> Json.arr(
            Json.obj(
              "type" -> "Text",
              "text" -> message
            )
          )
        ),
        "feedElementType" -> "FeedItem",
        "subjectId" -> subjectId
      )

      ws.url(feedUrl(userInfo, maybeCommunityId))
        .withHttpHeaders(AUTHORIZATION -> bearerAuth(auth))
        .post(json)
        .flatMap {
          on(CREATED) { response =>
            (response.json, instanceUrl(userInfo))
          }
        }
    }
  }

  def chatterPostLink(auth: String, linkUrl: String, maybeMessage: Option[String], maybeCommunityId: Option[String], maybeGroup: Option[String]): Future[(JsValue, String)] = {
    userinfo(auth).flatMap { userInfo =>
      val userId = (userInfo \ "user_id").as[String]
      val subjectId = maybeGroup.getOrElse(userId)

      val json = Json.obj(
        "capabilities" -> Json.obj(
          "link" -> Json.obj(
            "url" -> linkUrl
          )
        ),
        "feedElementType" -> "FeedItem",
        "subjectId" -> subjectId
      )

      val jsonWithMaybeMessage = maybeMessage.fold(json) { message =>
        val body = Json.obj(
          "body" -> Json.obj(
            "messageSegments" -> Json.arr(
              Json.obj(
                "type" -> "Text",
                "text" -> message
              )
            )
          )
        )

        json ++ body
      }

      ws.url(feedUrl(userInfo, maybeCommunityId))
        .withHttpHeaders(AUTHORIZATION -> bearerAuth(auth))
        .post(jsonWithMaybeMessage)
        .flatMap {
          on(CREATED) { createResponse => (createResponse.json, instanceUrl(userInfo)) }
        }
    }

  }

  // todo: max file size
  def chatterPostFile(auth: String, fileUrl: String, fileName: String, maybeMessage: Option[String], maybeCommunityId: Option[String], maybeGroup: Option[String]): Future[(JsValue, String)] = {
    userinfo(auth).flatMap { userInfo =>
      ws.url(fileUrl).get().flatMap { response =>
        val userId = (userInfo \ "user_id").as[String]
        val subjectId = maybeGroup.getOrElse(userId)

        val json = Json.obj(
          "capabilities" -> Json.obj(
            "content" -> Json.obj(
              "title" -> fileName
            )
          ),
          "feedElementType" -> "FeedItem",
          "subjectId" -> subjectId
        )

        val jsonWithMaybeMessage = maybeMessage.fold(json) { message =>
          val body = Json.obj(
            "body" -> Json.obj(
              "messageSegments" -> Json.arr(
                Json.obj(
                  "type" -> "Text",
                  "text" -> message
                )
              )
            )
          )

          json ++ body
        }

        val filePart = FilePart("feedElementFileUpload", fileName, Some(response.contentType), response.bodyAsSource)
        val jsonPart = FilePart("json", "", Some(JSON), Source.single(ByteString(jsonWithMaybeMessage.toString())))

        val parts = Source(filePart :: jsonPart :: List())

        ws.
          url(feedUrl(userInfo, maybeCommunityId)).
          withHttpHeaders(AUTHORIZATION -> bearerAuth(auth)).
          post(parts).
          flatMap { createResponse =>
            createResponse.status match {
              case CREATED =>
                Future.successful(createResponse.json, instanceUrl(userInfo))
              case _ =>
                Future.failed(ForceError(createResponse.json))
            }
          }
      }
    }
  }

  def insert(auth: String, sobject: String, json: JsValue): Future[JsValue] = {
    userinfo(auth).flatMap { userInfo =>
      ws.
        url(sobjectsUrl(userInfo) + sobject).
        withHttpHeaders(AUTHORIZATION -> bearerAuth(auth)).
        post(json).
        flatMap {
          on(CREATED)(_.json)
        }
    }
  }

  def update(auth: String, sobject: String, id: String, json: JsObject): Future[Unit] = {
    userinfo(auth).flatMap { userInfo =>
      ws.
        url(sobjectsUrl(userInfo) + sobject + "/" + id).
        withHttpHeaders(AUTHORIZATION -> bearerAuth(auth)).
        patch(json).
        flatMap {
          on(NO_CONTENT)(_ => Unit)
        }
    }
  }

  // todo: move action code out of here
  def sobjectOptions(filter: String): Action[JsValue] = Action.async(parse.json) { request =>

    request.headers.get(AUTHORIZATION).fold {
      Future.successful(Unauthorized(ByteString.empty))
    } { auth =>

      userinfo(auth).flatMap { userinfo =>
        val url = sobjectsUrl(userinfo)

        val queryRequest = ws.url(url).withHttpHeaders(AUTHORIZATION -> bearerAuth(auth)).get()

        queryRequest.flatMap {
          on(OK) { queryResponse =>
            val sobjects = (queryResponse.json \ "sobjects").as[Seq[JsObject]]

            // todo: use a JSON transformer
            val options = sobjects.filter(_.\(filter).as[Boolean]).map { json =>
              Json.obj("label" -> (json \ "label").as[String], "value" -> (json \ "name").as[String])
            } sortBy (_.\("label").as[String])

            Ok(
              Json.obj(
                "data" -> options
              )
            )
          }
        }
      } recoverWith standardErrorHandler(auth)
    }
  }

  def chatterGroups(auth: String, userInfo: JsValue): Future[JsArray] = {
    // /chatter/users/userId/groups
    val userId = (userInfo \ "user_id").as[String]
    val url = usersUrl(userInfo) + s"/$userId/groups"

    ws.url(url)
      .withHttpHeaders(AUTHORIZATION -> bearerAuth(auth))
      .withQueryStringParameters("pageSize" -> 250.toString)
      .get()
      .flatMap {
        on(OK)(_.json.\("groups").as[JsArray])
      }
  }

  def query(auth: String, userInfo: JsValue, soql: String): Future[JsValue] = {
    ws.url(queryUrl(userInfo))
      .withHttpHeaders(AUTHORIZATION -> bearerAuth(auth))
      .withQueryStringParameters("q" -> soql)
      .get()
      .flatMap(on(OK)(_.json))
  }

  def opportunitiesWon(auth: String, userInfo: JsValue, limit: Int): Future[JsObject] = {
    val soql =
      s"""
         |SELECT Id, LastModifiedDate, CloseDate, Name, Amount, Owner.Name
         |FROM Opportunity
         |WHERE IsWon = TRUE
         |ORDER BY LastModifiedDate DESC
         |LIMIT $limit
                """.stripMargin

    query(auth, userInfo, soql).map(_.as[JsObject])
  }

  def communities(auth: String, userInfo: JsValue): Future[JsArray] = {
    // /services/data/v35.0/connect/communities

    ws.url(restUrl(userInfo) + "connect/communities")
      .withHttpHeaders(AUTHORIZATION -> bearerAuth(auth))
      .get()
      .flatMap {
        on(OK)(_.json.\("communities").asOpt[JsArray].getOrElse(JsArray()))
      }
  }

  def communityGroups(auth: String, userInfo: JsValue, communityId: String): Future[JsArray] = {
    // /services/data/v35.0/connect/communities/communityId/chatter/groups/

    ws.url(restUrl(userInfo) + s"connect/communities/$communityId/chatter/groups")
      .withHttpHeaders(AUTHORIZATION -> bearerAuth(auth))
      .get()
      .flatMap {
        on(OK)(_.json.\("groups").asOpt[JsArray].getOrElse(JsArray()))
      }
  }

  def describe(auth: String, sobject: String): Future[JsValue] = {
    userinfo(auth).flatMap { userInfo =>
      ws.
        url(sobjectsUrl(userInfo) + sobject + "/describe").
        withHttpHeaders(AUTHORIZATION -> bearerAuth(auth)).
        get().
        flatMap(on(OK)(_.json))
    }
  }

  def queryUrl(value: JsValue) = (value \ "urls" \ "query").as[String].replace("{version}", API_VERSION)

  def sobjectsUrl(value: JsValue) = (value \ "urls" \ "sobjects").as[String].replace("{version}", API_VERSION)

  def restUrl(value: JsValue) = (value \ "urls" \ "rest").as[String].replace("{version}", API_VERSION)

  def usersUrl(value: JsValue) = (value \ "urls" \ "users").as[String].replace("{version}", API_VERSION)

  def instanceUrl(value: JsValue) = (value \ "profile").as[String].stripSuffix((value \ "user_id").as[String])

  def saveError(auth: String, error: String)(result: => Result): Future[Result] = {
    userinfo(auth).flatMap { userInfo =>
      val userId = (userInfo \ "user_id").as[String]
      redis.client.lpush(userId, error).map(_ => result)
    } recover {
      case e: Exception =>
        logger.error(e.getMessage)
        result
    }
  }

  def standardErrorHandler(auth: String): PartialFunction[Throwable, Future[Result]] = {
    case UnauthorizedException(message) =>
      val json = Json.obj(
        "errors" -> Json.arr(
          Json.obj(
            "status" -> message,
            "message" -> s"Authentication failed: $message"
          )
        )
      )
      Future.successful(Unauthorized(json))
    case fe: ForceError =>
      logger.info(fe.getStackTrace.mkString("\n"))
      logger.info(Json.asciiStringify(fe.json))
      logger.info(fe.getMessage)
      saveError(auth, fe.getMessage) {
        // transform error to ifttt
        BadRequest(
          Json.obj("errors" ->
            fe.json.as[Seq[JsObject]].map { error =>
              Json.obj(
                "status" -> (error \ "errorCode").as[JsValue],
                "message" -> (error \ "message").as[JsValue]
              )
            }
          )
        )
      }
    case e: Exception =>
      logger.error(e.getStackTrace.mkString("\n"))
      logger.error(e.getMessage)
      saveError(auth, e.getMessage) {
        InternalServerError(Json.obj("error" -> e.getMessage))
      }
  }
}

object Force {

  // todo: as of version 36.0 the multipart upload to the chatter feed is no longer supported:
  // https://developer.salesforce.com/docs/atlas.en-us.chatterapi.meta/chatterapi/intro_input.htm
  val API_VERSION = "34.0"

  val ENV_PROD = "prod"
  val ENV_SANDBOX = "sandbox"
  val SALESFORCE_ENV = "salesforce-env"

  case class UnauthorizedException(message: String) extends Exception {
    override def getMessage = message
  }

  case class ForceError(json: JsValue) extends Exception {
    override def getMessage = {
      (json \\ "message").map(_.as[String]).mkString
    }
  }

  object ForceError {
    def apply(message: String): ForceError = new ForceError(
      Json.arr(
        Json.obj(
          "message" -> message
        )
      )
    )
  }

}
