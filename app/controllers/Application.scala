package controllers

import play.api.libs.Crypto
import play.api.mvc.{Action, Controller}
import utils.{Force, Global}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Application extends Controller {

  def index() = Action {
    Ok(views.html.index())
  }

  def install = Action {
    Ok(views.html.install())
  }

  def help = Action {
    Ok(views.html.help())
  }

  def errors = Action.async { request =>
    request.flash.get("enc_access_token").fold {
      val redirUrl = routes.OAuth2.authorized().absoluteURL(secure = request.secure)(request)
      val qs = s"client_id=${Force.salesforceOauthKey}&state=local-errors&response_type=code&prompt=login&redirect_uri=$redirUrl"

      Future.successful(Ok(views.html.authorizeErrors(qs)))
    } { encAccessToken =>
      val accessToken = Crypto.decryptAES(encAccessToken)
      val auth = s"Bearer $accessToken"
      Force.userinfo(auth).flatMap { userInfo =>
        val userId = (userInfo \ "user_id").as[String]

        Global.redis.lrange[String](userId, 0, -1).map { errors =>
          val friendlyErrors = errors.distinct.map {
            case error if error.contains("Object type 'ifttt__IFTTT_Event__c' is not supported") =>
              views.html.packageNotFound().toString()
            case error =>
              views.html.standardError(error).toString()
          }

          Ok(views.html.errors(friendlyErrors, encAccessToken))
        }
      } recoverWith Force.standardErrorHandler(auth)
    }
  }

  def errorsClear = Action.async(parse.urlFormEncoded) { request =>
    request.body.get("enc_access_token").flatMap(_.headOption).fold {
      Future.successful(Unauthorized("No access token in scope"))
    } { encAccessToken =>
      val accessToken = Crypto.decryptAES(encAccessToken)
      val auth = s"Bearer $accessToken"
      Force.userinfo(auth).flatMap { userInfo =>
        val userId = (userInfo \ "user_id").as[String]

        Global.redis.del(userId).map { errors =>
          Redirect(routes.Application.errors()).flashing("enc_access_token" -> encAccessToken)
        }
      } recoverWith Force.standardErrorHandler(auth)
    }
  }

}
