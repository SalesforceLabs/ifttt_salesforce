package controllers

import play.api.libs.ws.WS
import play.api.mvc.{Action, Controller}
import play.api.Play.current
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object OAuth2 extends Controller {

  def authorize = Action { request =>
    val qs = request.queryString - "scope"
    Redirect("https://login.salesforce.com/services/oauth2/authorize", qs)
  }

  def token = Action.async(parse.urlFormEncoded) { request =>
    val requestHeaders = request.headers.toSimpleMap.toSeq

    val tokenFuture = WS.
      url("https://login.salesforce.com/services/oauth2/token").
      withHeaders(requestHeaders: _*).
      post(request.body)

    tokenFuture.map { response =>
      val maybeRefreshToken = request.body.get("refresh_token").flatMap(_.headOption)
      val json = maybeRefreshToken.fold(response.json) { refreshToken =>
        val jsonResult = response.json.transform {
          // todo: find a cleaner way to add a value to the json
          __.json.update(__.read[JsObject].map(_ ++ Json.obj("refresh_token" -> refreshToken)))
        }
        jsonResult.get
      }
      Ok(json)
    }

  }

}
