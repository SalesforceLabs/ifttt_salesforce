package utils

import org.apache.commons.codec.digest.DigestUtils
import play.api.http.{Status, HeaderNames}
import play.api.libs.json.JsValue
import play.api.libs.ws.{WSResponse, WS}
import play.api.Play.current
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
          Future.successful(response, None)
      }
    }
  }

  def queryUrl(value: JsValue) = (value \ "urls" \ "query").as[String].replace("{version}", API_VERSION)

  def sobjectsUrl(value: JsValue) = (value \ "urls" \ "sobjects").as[String].replace("{version}", API_VERSION)

  def instanceUrl(value: JsValue) =  (value \ "profile").as[String].stripSuffix((value \ "user_id").as[String])

}
