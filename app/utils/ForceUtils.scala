package utils

import org.apache.commons.codec.digest.DigestUtils
import play.api.Logger
import play.api.http.{Status, HeaderNames}
import play.api.libs.json.{Json, JsObject, JsValue}
import play.api.libs.ws.{WSResponse, WS}
import play.api.Play.current
import play.api.mvc.{Result, BodyParsers, Action, Results}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

object ForceUtils {

  val API_VERSION = "30.0"

  val ENV_PROD = "prod"
  val ENV_SANDBOX = "sandbox"
  val SALESFORCE_ENV = "salesforce-env"

  // todo: maybe put an in-memory cache here since this can get called a lot
  def userinfo(auth: String): Future[JsValue] = {
    val bearerAuth = if (auth.startsWith("Bearer ")) auth else s"Bearer $auth"

    Global.redis.get[String](DigestUtils.sha1Hex(bearerAuth)).flatMap { maybeEnv =>
      val env = maybeEnv.getOrElse(ENV_PROD)
      WS.url(userinfoUrl(env)).withHeaders(HeaderNames.AUTHORIZATION -> bearerAuth).get().flatMap { userInfoResponse =>
        userInfoResponse.status match {
          case Status.OK =>
            Future.successful(userInfoResponse.json)
          case Status.UNAUTHORIZED | Status.FORBIDDEN =>
            Future.failed(UnauthorizedException(userInfoResponse.body))
          case _ =>
            Future.failed(new Exception(userInfoResponse.body))
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

  def chatterPost(auth: String, message: String): Future[(JsValue, String)] = {
    userinfo(auth).flatMap { userInfo =>
      val userId = (userInfo \ "user_id").as[String]
      val instanceUrl = (userInfo \ "profile").as[String].stripSuffix(userId)
      val feedsUrl = (userInfo \ "urls" \ "feeds").as[String].replace("{version}", API_VERSION)
      WS.
        url(feedsUrl + "/news/me/feed-items").
        withHeaders(HeaderNames.AUTHORIZATION -> auth).
        post(Map("text" -> Seq(message))).
        flatMap { createResponse =>
          createResponse.status match {
            case Status.CREATED =>
              Future.successful(createResponse.json, instanceUrl)
            case _ =>
              Future.failed(new Exception(s"Could not post on Chatter: ${createResponse.body}"))
          }
        }
    }
  }

  def insert(auth: String, sobject: String, json: JsValue): Future[JsValue] = {
    userinfo(auth).flatMap { userInfo =>
      WS.
        url(sobjectsUrl(userInfo) + sobject).
        withHeaders(HeaderNames.AUTHORIZATION -> auth).
        post(json).
        flatMap { response =>
          response.status match {
            case Status.CREATED =>
              Future.successful(response.json)
            case _ =>
              Future.failed(new Exception(s"Could not insert a record: ${response.body}"))
          }
        }
    }
  }

  def sobjectOptions(filter: String): Action[JsValue] = Action.async(BodyParsers.parse.json) { request =>

    request.headers.get(HeaderNames.AUTHORIZATION).fold {
      Future.successful(Results.Unauthorized(""))
    } { auth =>

      ForceUtils.userinfo(auth).flatMap { userinfo =>
        val url = sobjectsUrl(userinfo)

        val queryRequest = WS.url(url).withHeaders(HeaderNames.AUTHORIZATION -> auth).get()

        queryRequest.map { queryResponse =>

          val sobjects = (queryResponse.json \ "sobjects").as[Seq[JsObject]]

          // todo: use a JSON transformer
          val options = sobjects.filter(_.\(filter).as[Boolean]).map { json =>
            Json.obj("label" -> (json \ "label").as[String], "value" -> (json \ "name").as[String])
          } sortBy (_.\("label").as[String])

          Results.Ok(
            Json.obj(
              "data" -> options
            )
          )
        }
      } recoverWith standardErrorHandler(auth)
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
    case e: Exception =>
      ForceUtils.saveError(auth, e.getMessage) {
        Results.InternalServerError(Json.obj("error" -> e.getMessage))
      }
  }

  case class UnauthorizedException(message: String) extends Exception {
    override def getMessage = message
  }

}
