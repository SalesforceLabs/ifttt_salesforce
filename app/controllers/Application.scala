package controllers

import play.api.Play
import play.api.libs.Crypto
import play.api.mvc.{Action, Controller}
import utils.{Global, ForceUtils}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Application extends Controller {

  lazy val managedPackageId = Play.current.configuration.getString("salesforce.managed-package-id").get

  def index() = Action {
    Ok(views.html.index(managedPackageId))
  }

  def errors = Action.async { request =>
    request.flash.get("enc_access_token").fold {
      val redirUrl = routes.OAuth2.authorized().absoluteURL(secure = request.secure)(request)
      val qs = s"client_id=${OAuth2.salesforceOauthKey}&state=local-errors&response_type=code&prompt=login&redirect_uri=$redirUrl"

      Future.successful(Ok(views.html.authorize(qs)))
    } { encAccessToken =>
      val accessToken = Crypto.decryptAES(encAccessToken)
      val auth = s"Bearer $accessToken"
      ForceUtils.userinfo(auth).flatMap { userInfoResponse =>
        userInfoResponse.status match {
          case OK =>
            val userId = (userInfoResponse.json \ "user_id").as[String]

            Global.redis.lrange[String](userId, 0, -1).map { errors =>
              val encAccessToken = Crypto.encryptAES(accessToken)
              Ok(views.html.errors(errors, encAccessToken))
            }
          case _ =>
            Future.successful(InternalServerError(userInfoResponse.body))
        }
      }
    }
  }

  def errorsClear = Action.async(parse.urlFormEncoded) { request =>
    request.body.get("enc_access_token").flatMap(_.headOption).fold {
      Future.successful(Unauthorized("No access token in scope"))
    } { encAccessToken =>
      val accessToken = Crypto.decryptAES(encAccessToken)
      val auth = s"Bearer $accessToken"
      ForceUtils.userinfo(auth).flatMap { userInfoResponse =>
        userInfoResponse.status match {
          case OK =>
            val userId = (userInfoResponse.json \ "user_id").as[String]

            Global.redis.del(userId).map { errors =>
              Redirect(routes.Application.errors()).flashing("enc_access_token" -> encAccessToken)
            }
          case _ =>
            Future.successful(InternalServerError(userInfoResponse.body))
        }
      }
    }
  }

}
