package utils

import org.apache.commons.codec.digest.DigestUtils
import play.api.http.{Status, HeaderNames}
import play.api.libs.json.{Json, JsObject, JsValue}
import play.api.libs.ws.{WSResponse, WS}
import play.api.Play.current
import play.api.mvc._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object ForceUtils {

  val API_VERSION = "30.0"

  val ENV_PROD = "prod"
  val ENV_SANDBOX = "sandbox"
  val SALESFORCE_ENV = "salesforce-env"

  def userinfo(auth: String): Future[WSResponse] = {
    Global.redis.get[String](DigestUtils.sha1Hex(auth)).flatMap { maybeEnv =>
      val env = maybeEnv.getOrElse(ENV_PROD)
      WS.url(userinfoUrl(env)).withHeaders(HeaderNames.AUTHORIZATION -> auth).get()
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

  def chatterPost(auth: String, message: String): Future[(WSResponse, Option[String])] = {
    userinfo(auth).flatMap { response =>

      response.status match {
        case Status.OK =>
          val userId = (response.json \ "user_id").as[String]
          val instanceUrl = (response.json \ "profile").as[String].stripSuffix(userId)
          val feedsUrl = (response.json \ "urls" \ "feeds").as[String].replace("{version}", API_VERSION)
          WS.
            url(feedsUrl + "/news/me/feed-items").
            withHeaders(HeaderNames.AUTHORIZATION -> auth).
            post(Map("text" -> Seq(message))).
            map((_, Some(instanceUrl)))

        case Status.FORBIDDEN =>
          val n: Option[String] = None
          Future.successful(response, n)
      }
    }
  }

  def insert(auth: String, sobject: String, json: JsValue): Future[WSResponse] = {
    userinfo(auth).flatMap { response =>

      response.status match {
        case Status.OK =>
          WS.
            url(sobjectsUrl(response.json) + sobject).
            withHeaders(HeaderNames.AUTHORIZATION -> auth).
            post(json)

        case Status.FORBIDDEN =>
          Future.successful(response)
      }
    }

  }

  def sobjectOptions(): Action[JsValue] = Action.async(BodyParsers.parse.json) { request =>

    request.headers.get(HeaderNames.AUTHORIZATION).map { auth =>

      ForceUtils.userinfo(auth).flatMap { userinfoResponse =>

        userinfoResponse.status match {
          case Status.OK =>
            val url = ForceUtils.sobjectsUrl(userinfoResponse.json)

            val queryRequest = WS.url(url).withHeaders(HeaderNames.AUTHORIZATION -> auth).get()

            queryRequest.map { queryResponse =>

              val sobjects = (queryResponse.json \ "sobjects").as[Seq[JsObject]]

              // todo: use a JSON transformer
              val options = sobjects.filter(_.\("queryable").as[Boolean]).map { json =>
                Json.obj("label" -> (json \ "label").as[String], "value" -> (json \ "name").as[String])
              }

              Results.Ok(
                Json.obj(
                  "data" -> options
                )
              )
            }
          case Status.FORBIDDEN =>
            val json = Json.obj(
              "errors" -> Json.arr(
                Json.obj(
                  "status" -> userinfoResponse.body,
                  "message" -> ("Authentication failed: " + userinfoResponse.body)
                )
              )
            )
            Future.successful(Results.Unauthorized(json))
          case _ =>
            Future.successful(Results.Status(userinfoResponse.status)(userinfoResponse.body))
        }
      }

    } getOrElse Future.successful(Results.Unauthorized(""))
  }


  def queryUrl(value: JsValue) = (value \ "urls" \ "query").as[String].replace("{version}", API_VERSION)

  def sobjectsUrl(value: JsValue) = (value \ "urls" \ "sobjects").as[String].replace("{version}", API_VERSION)

  def instanceUrl(value: JsValue) =  (value \ "profile").as[String].stripSuffix((value \ "user_id").as[String])

}
