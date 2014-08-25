package utils

import play.api.http.{Status, HeaderNames}
import play.api.libs.json.JsValue
import play.api.libs.ws.{WSResponse, WS}
import play.api.Play.current
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object ForceUtils {

  val API_VERSION = "30.0"

  def userinfo(auth: String) = {
    WS.
      url("https://login.salesforce.com/services/oauth2/userinfo").
      withHeaders(HeaderNames.AUTHORIZATION -> auth).
      get()
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

  def instanceUrl(value: JsValue) =  (value \ "profile").as[String].stripSuffix("/" + (value \ "user_id").as[String])

}
