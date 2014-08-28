package controllers

import org.apache.commons.codec.digest.DigestUtils
import play.api.libs.ws.WS
import play.api.mvc.{Request, RequestHeader, Action, Controller}
import play.api.Play.current
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import utils.{ForceUtils, Global}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object OAuth2 extends Controller {



  /*
  Step 1

  This is the first page that is rendered in the oauth flow.  From here the user needs to select either prod or sandbox.
   */
  def authorize = Action { request =>
    if (request.queryString.contains("client_id") && request.queryString.contains("response_type") && request.queryString.contains("state")) {
      val qsMap = request.queryString - "scope"
      val qsMapWithRedir = qsMap.updated("redirect_uri", Seq(routes.OAuth2.authorized().absoluteURL(secure =  true)(request)))
      import java.net.URLEncoder
      val qs = Option(qsMapWithRedir).filterNot(_.isEmpty).map { params =>
        params.toSeq.flatMap { pair =>
          pair._2.map(value => pair._1 + "=" + URLEncoder.encode(value, "utf-8"))
        }.mkString("&")
      }.getOrElse("")

      Ok(views.html.authorize(qs))
    }
    else {
      Redirect(routes.Application.index())
    }
  }

  /*
  Step 2 - prod

  The user gets a cookie indicating they are using prod.
   */
  def authorizeProd() = Action { request =>
    Redirect("https://login.salesforce.com/services/oauth2/authorize", request.queryString).withSession(ForceUtils.SALESFORCE_ENV -> ForceUtils.ENV_PROD)
  }

  /*
  Step 2 - sandbox

  The user gets a cookie indicating they are using sandbox.
   */
  def authorizeSandbox() = Action { request =>
    Redirect("https://test.salesforce.com/services/oauth2/authorize", request.queryString).withSession(ForceUtils.SALESFORCE_ENV -> ForceUtils.ENV_SANDBOX)
  }

  // Step 3 - User logs in and authorizes the app on salesforce

  /*
  Step 4

  Salesforce redirects here and we need to associate their code with the salesforce env
   */
  def authorized() = Action { request =>
    val maybeSalesforceEnv = request.session.get(ForceUtils.SALESFORCE_ENV)
    val maybeCode = request.queryString.get("code").flatMap(_.headOption)

    val maybeRedir = for {
      salesforceEnv <- maybeSalesforceEnv
      code <- maybeCode
    } yield {
      Global.redis.set(DigestUtils.sha1Hex(code), salesforceEnv)
      Redirect(s"https://ifttt.com/channels/${Global.ifffChannelId}/authorize", request.queryString)
    }

    maybeRedir.getOrElse(BadRequest("code and salesforce-env are required"))
  }

  /*
  Step 5

  IFTTT calls here with the code or a refresh_token
   */
  def token = Action.async(parse.urlFormEncoded) { request =>
    val maybeRefreshToken = request.body.get("refresh_token").flatMap(_.headOption)
    val maybeCode = request.body.get("code").flatMap(_.headOption)

    (maybeRefreshToken, maybeCode) match {
      case (Some(refreshToken), _) =>
      // auth with refresh token
        tokenRefresh(request, refreshToken).map(Ok(_))
      case (_, Some(code)) =>
      // auth with code
        tokenCode(request, code).map(Ok(_))
      case _ =>
        Future.successful(BadRequest("auth with either a code or a refresh_token"))
    }
  }

  private def tokenRefresh(request: Request[Map[String, Seq[String]]], refreshToken: String): Future[JsValue] = {
    Global.redis.get[String](DigestUtils.sha1Hex(refreshToken)).flatMap { maybeEnv =>
      val env = maybeEnv.getOrElse(ForceUtils.ENV_PROD)

      val tokenFuture = WS.url(ForceUtils.loginUrl(env)).post(request.body)

      tokenFuture.flatMap { response =>
        val accessToken = "Bearer " + (response.json \ "access_token").as[String] // add the Bearer prefix since that is how we will retrieve it

        Global.redis.set(DigestUtils.sha1Hex(refreshToken), env).map { _ =>
          // adding the refresh token back into the json because ifttt needs it
          val jsonResult = response.json.transform {
            // todo: find a cleaner way to add a value to the json
            __.json.update(__.read[JsObject].map(_ ++ Json.obj("refresh_token" -> refreshToken)))
          }
          jsonResult.get
        }
      }
    }
  }

  private def tokenCode(request: Request[Map[String, Seq[String]]], code: String): Future[JsValue] = {
    Global.redis.get[String](DigestUtils.sha1Hex(code)).flatMap { maybeEnv =>
      val env = maybeEnv.getOrElse(ForceUtils.ENV_PROD)

      val requestHeaders = request.headers.toSimpleMap.toSeq
      val bodyWithRedir = request.body.updated("redirect_uri", Seq(routes.OAuth2.authorized().absoluteURL(secure =  true)(request)))

      val tokenFuture = WS.url(ForceUtils.loginUrl(env)).withHeaders(requestHeaders: _*).post(bodyWithRedir)

      tokenFuture.flatMap { response =>
        val newRefreshToken = (response.json \ "refresh_token").as[String]
        val accessToken = "Bearer " + (response.json \ "access_token").as[String] // add the Bearer prefix since that is how we will retrieve it

        // store the hash of the refresh token with the env in redis
        Global.redis.set(DigestUtils.sha1Hex(newRefreshToken), env).flatMap { _ =>
          // store the hash of the access token with the env in redis
          Global.redis.set(DigestUtils.sha1Hex(accessToken), env).map(_ => response.json)
        }
      }
    }
  }

}
