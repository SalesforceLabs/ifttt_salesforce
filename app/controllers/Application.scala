package controllers

import play.api.Play
import play.api.libs.Crypto
import play.api.mvc.{Action, Controller}
import utils.{ForceUtils, Global}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Application extends Controller {

  def index() = Action {
    Ok(views.html.index())
  }

  def errors = Action.async { request =>
    request.flash.get("enc_access_token").fold {
      val redirUrl = routes.OAuth2.authorized().absoluteURL(secure = request.secure)(request)
      val qs = s"client_id=${ForceUtils.salesforceOauthKey}&state=local-errors&response_type=code&prompt=login&redirect_uri=$redirUrl"

      Future.successful(Ok(views.html.authorize(qs, ForceUtils.managedPackageId)))
    } { encAccessToken =>
      val accessToken = Crypto.decryptAES(encAccessToken)
      val auth = s"Bearer $accessToken"
      ForceUtils.userinfo(auth).flatMap { userInfo =>
        val userId = (userInfo \ "user_id").as[String]

        Global.redis.lrange[String](userId, 0, -1).map { errors =>
          val encAccessToken = Crypto.encryptAES(accessToken)
          Ok(views.html.errors(errors, encAccessToken))
        }
      } recoverWith ForceUtils.standardErrorHandler(auth)
    }
  }

  def errorsClear = Action.async(parse.urlFormEncoded) { request =>
    request.body.get("enc_access_token").flatMap(_.headOption).fold {
      Future.successful(Unauthorized("No access token in scope"))
    } { encAccessToken =>
      val accessToken = Crypto.decryptAES(encAccessToken)
      val auth = s"Bearer $accessToken"
      ForceUtils.userinfo(auth).flatMap { userInfo =>
        val userId = (userInfo \ "user_id").as[String]

        Global.redis.del(userId).map { errors =>
          Redirect(routes.Application.errors()).flashing("enc_access_token" -> encAccessToken)
        }
      } recoverWith ForceUtils.standardErrorHandler(auth)
    }
  }

}
