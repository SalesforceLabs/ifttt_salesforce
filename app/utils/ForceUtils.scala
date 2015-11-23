package utils

import com.ning.http.client._
import com.ning.http.multipart.FilePart
import com.ning.http.multipart.Part
import com.ning.http.multipart.StringPart
import com.ning.http.multipart._
import org.apache.commons.codec.digest.DigestUtils
import play.api.libs.ws.ning.NingWSResponse
import play.api.{Play, Logger}
import play.api.http.{ContentTypes, Status, HeaderNames}
import play.api.libs.json._
import play.api.libs.ws.{WSResponse, WS}
import play.api.Play.current
import play.api.mvc._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Promise, Future}
import scala.util.Try

object ForceUtils {

  val API_VERSION = "34.0"

  val ENV_PROD = "prod"
  val ENV_SANDBOX = "sandbox"
  val SALESFORCE_ENV = "salesforce-env"

  lazy val salesforceOauthKey = Play.current.configuration.getString("salesforce.oauth.key").get
  lazy val salesforceOauthSecret = Play.current.configuration.getString("salesforce.oauth.secret").get

  lazy val managedPackageId = Play.current.configuration.getString("salesforce.managed-package-id").get

  private def bearerAuth(auth: String) = if (auth.startsWith("Bearer ")) auth else s"Bearer $auth"

  // todo: maybe put an in-memory cache here since this can get called a lot
  def userinfo(auth: String): Future[JsValue] = {
    Global.redis.get[String](DigestUtils.sha1Hex(bearerAuth(auth))).flatMap { maybeEnv =>
      val env = maybeEnv.getOrElse(ENV_PROD)
      WS.url(userinfoUrl(env)).withHeaders(HeaderNames.AUTHORIZATION -> bearerAuth(auth)).get().flatMap { userInfoResponse =>
        userInfoResponse.status match {
          case Status.OK =>
            Future.successful(userInfoResponse.json)
          case Status.UNAUTHORIZED | Status.FORBIDDEN =>
            Future.failed(UnauthorizedException(userInfoResponse.body))
          case _ =>
            val jsonTry = Try(userInfoResponse.json)
            Future.failed(jsonTry.map(ForceError).getOrElse(new Exception("Could not get user info: " + userInfoResponse.body)))
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

  def login(env: String, username: String, password: String): Future[JsValue] = {

    val body = Map(
      "grant_type" -> "password",
      "client_id" -> salesforceOauthKey,
      "client_secret" -> salesforceOauthSecret,
      "username" -> username,
      "password" -> password
    ).mapValues(Seq(_))

    WS.url(loginUrl(env)).post(body).flatMap { response =>
      response.status match {
        case Status.OK => Future.successful(response.json)
        case _ => Future.failed(ForceError(response.json))
      }
    }

  }

  def chatterPostMessage(auth: String, message: String, groupId: Option[String]): Future[(JsValue, String)] = {
    userinfo(auth).flatMap { userInfo =>
      val userId = (userInfo \ "user_id").as[String]
      val subjectId = groupId.getOrElse(userId)
      val instanceUrl = (userInfo \ "profile").as[String].stripSuffix(userId)
      val restUrl = (userInfo \ "urls" \ "rest").as[String].replace("{version}", API_VERSION)
      val url = restUrl + "chatter/feed-elements"

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

      WS.url(url)
        .withHeaders(HeaderNames.AUTHORIZATION -> bearerAuth(auth))
        .post(json)
        .flatMap { createResponse =>
          createResponse.status match {
            case Status.CREATED =>
              Future.successful(createResponse.json, instanceUrl)
            case _ =>
              Future.failed(ForceError(createResponse.json))
          }
        }
    }
  }

  def chatterPostLink(auth: String, linkUrl: String, maybeMessage: Option[String], maybeGroup: Option[String]): Future[(JsValue, String)] = {
    userinfo(auth).flatMap { userInfo =>
      val userId = (userInfo \ "user_id").as[String]
      val subjectId = maybeGroup.getOrElse(userId)
      val instanceUrl = (userInfo \ "profile").as[String].stripSuffix(userId)
      val restUrl = (userInfo \ "urls" \ "rest").as[String].replace("{version}", API_VERSION)
      val url = restUrl + "chatter/feed-elements"

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

      WS.url(url)
        .withHeaders(HeaderNames.AUTHORIZATION -> bearerAuth(auth))
        .post(jsonWithMaybeMessage)
        .flatMap { createResponse =>
        createResponse.status match {
          case Status.CREATED =>
            Future.successful(createResponse.json, instanceUrl)
          case _ =>
            Future.failed(ForceError(createResponse.json))
        }
      }
    }

  }

  private def postMultiPart(url: String, headers: Seq[(String, String)], bodyParts: Seq[Part]): Future[WSResponse] = {
    val client = WS.client.underlying.asInstanceOf[AsyncHttpClient]

    val builder = client.preparePost(url)

    builder.setHeader(HeaderNames.CONTENT_TYPE, "multipart/form-data")
    headers.foreach((builder.setHeader _).tupled)
    bodyParts.foreach(builder.addBodyPart)

    val request = builder.build()

    val result = Promise[NingWSResponse]()

      client.executeRequest(request, new AsyncCompletionHandler[Response]() {
        override def onCompleted(response: Response) = {
          result.trySuccess(NingWSResponse(response))
          response
        }

        override def onThrowable(t: Throwable) = {
          result.tryFailure(t)
        }
      })

    result.future
  }

  class JsonPart(name: String, value: JsValue) extends StringPart(name, value.toString()) {
    override def getContentType: String = ContentTypes.JSON
  }

  // todo: max file size
  def chatterPostFile(auth: String, fileUrl: String, fileName: String, maybeMessage: Option[String], maybeGroup: Option[String]): Future[(JsValue, String)] = {
    userinfo(auth).flatMap { userInfo =>
      WS.url(fileUrl).get().flatMap { response =>
        val fileBytes = response.underlying[com.ning.http.client.Response].getResponseBodyAsBytes

        val userId = (userInfo \ "user_id").as[String]
        val subjectId = maybeGroup.getOrElse(userId)
        val instanceUrl = (userInfo \ "profile").as[String].stripSuffix(userId)
        val restUrl = (userInfo \ "urls" \ "rest").as[String].replace("{version}", API_VERSION)
        val url = restUrl + "chatter/feed-elements"

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

        val jsonPart = new JsonPart("json", jsonWithMaybeMessage)

        val filePartSource = new ByteArrayPartSource(fileName, fileBytes)

        val filePart = new FilePart("feedElementFileUpload", filePartSource)

        postMultiPart(url, Seq(HeaderNames.AUTHORIZATION -> bearerAuth(auth)), Seq(jsonPart, filePart)).flatMap { createResponse =>
          createResponse.status match {
            case Status.CREATED =>
              Future.successful(createResponse.json, instanceUrl)
            case _ =>
              Future.failed(ForceError(createResponse.json))
          }
        }
      }
    }
  }

  def insert(auth: String, sobject: String, json: JsValue): Future[JsValue] = {
    userinfo(auth).flatMap { userInfo =>
      WS.
        url(sobjectsUrl(userInfo) + sobject).
        withHeaders(HeaderNames.AUTHORIZATION -> bearerAuth(auth)).
        post(json).
        flatMap { response =>
          response.status match {
            case Status.CREATED =>
              Future.successful(response.json)
            case _ =>
              Future.failed(ForceError(response.json))
          }
        }
    }
  }

  // todo: move action code out of here
  def sobjectOptions(filter: String): Action[JsValue] = Action.async(BodyParsers.parse.json) { request =>

    request.headers.get(HeaderNames.AUTHORIZATION).fold {
      Future.successful(Results.Unauthorized(""))
    } { auth =>

      ForceUtils.userinfo(auth).flatMap { userinfo =>
        val url = sobjectsUrl(userinfo)

        val queryRequest = WS.url(url).withHeaders(HeaderNames.AUTHORIZATION -> bearerAuth(auth)).get()

        queryRequest.flatMap { queryResponse =>

          queryResponse.status match {
            case Status.OK =>
              val sobjects = (queryResponse.json \ "sobjects").as[Seq[JsObject]]

              // todo: use a JSON transformer
              val options = sobjects.filter(_.\(filter).as[Boolean]).map { json =>
                Json.obj("label" -> (json \ "label").as[String], "value" -> (json \ "name").as[String])
              } sortBy (_.\("label").as[String])

              Future.successful(
                Results.Ok(
                  Json.obj(
                    "data" -> options
                  )
                )
              )
            case _ =>
              Future.failed(ForceError(queryResponse.json))
          }
        }
      } recoverWith standardErrorHandler(auth)
    }
  }

  def chatterGroups(auth: String): Future[JsArray] = {
    // /chatter/users/userId/groups
    userinfo(auth).flatMap { userInfo =>
      val userId = (userInfo \ "user_id").as[String]
      val instanceUrl = (userInfo \ "profile").as[String].stripSuffix(userId)
      val usersUrl = (userInfo \ "urls" \ "users").as[String].replace("{version}", API_VERSION)
      val url = usersUrl + s"/$userId/groups"

      WS.url(url)
        .withHeaders(HeaderNames.AUTHORIZATION -> bearerAuth(auth))
        .withQueryString("pageSize" -> 250.toString)
        .get()
        .flatMap { response =>
        response.status match {
          case Status.OK =>
            Future.successful((response.json \ "groups").as[JsArray])
          case _ =>
            Future.failed(ForceError(response.json))
        }
      }
    }
  }

  def query(auth: String, soql: String): Future[(JsValue, JsValue)] = {
    userinfo(auth).flatMap { userInfo =>
      WS.url(queryUrl(userInfo))
        .withHeaders(HeaderNames.AUTHORIZATION -> bearerAuth(auth))
        .withQueryString("q" -> soql)
        .get()
        .flatMap { response =>
          response.status match {
            case Status.OK =>
              Future.successful(userInfo, response.json)
            case _ =>
              Future.failed(ForceError(response.json))
          }
        }
    }
  }


  def queryUrl(value: JsValue) = (value \ "urls" \ "query").as[String].replace("{version}", API_VERSION)

  def sobjectsUrl(value: JsValue) = (value \ "urls" \ "sobjects").as[String].replace("{version}", API_VERSION)

  def instanceUrl(value: JsValue) =  (value \ "profile").as[String].stripSuffix((value \ "user_id").as[String])

  def saveError(auth: String, error: String)(result: => Result): Future[Result] = {
    userinfo(auth).flatMap { userInfo =>
      val userId = (userInfo \ "user_id").as[String]
      Global.redis.lpush(userId, error).map(_ => result)
    } recover {
      case e: Exception =>
        Logger.error(e.getMessage)
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
      Future.successful(Results.Unauthorized(json))
    case fe: ForceError =>
      Logger.info(fe.getStackTrace.mkString)
      Logger.info(fe.json.toString)
      Logger.info(fe.getMessage)
      ForceUtils.saveError(auth, fe.getMessage) {
        // transform error to ifttt
        Results.BadRequest(
          Json.obj("errors" ->
            fe.json.as[Seq[JsObject]].map { error =>
              Json.obj(
                "status" -> (error \ "errorCode"),
                "message" -> (error \ "message")
              )
            }
          )
        )
      }
    case e: Exception =>
      Logger.error(e.getStackTrace.mkString)
      Logger.error(e.getMessage)
      ForceUtils.saveError(auth, e.getMessage) {
        Results.InternalServerError(Json.obj("error" -> e.getMessage))
      }
  }

  case class UnauthorizedException(message: String) extends Exception {
    override def getMessage = message
  }

  case class ForceError(json: JsValue) extends Exception {
    override def getMessage = {
      (json \\ "message").map(_.as[String]).mkString
    }
  }

}
