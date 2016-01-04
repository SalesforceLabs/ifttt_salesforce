package controllers.ifttt.v1

import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._
import play.api.mvc.{Action, Controller}
import utils.{ForceUtils, Global}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object User extends Controller {

  def info = Action.async { implicit request =>
    request.headers.get(AUTHORIZATION).fold {
      Future.successful(Unauthorized("Request did not contain an Authorization header"))
    } { auth =>
      ForceUtils.userinfo(auth).map { userinfo =>
        val authToken = auth.stripPrefix("Bearer ")

        val instanceUrl = ForceUtils.instanceUrl(userinfo)

        val userId = (userinfo \ "user_id").as[String]

        val orgId = (userinfo \ "organization_id").as[String]

        // add the user to the watchers in this org in order to support real-time notifications
        Global.redis.sadd(orgId, userId)

        val jsonResult = userinfo.transform {
          val reads = {
            (__ \ 'data \ 'id).json.copyFrom((__ \ 'user_id).json.pick) and
              (__ \ 'data \ 'name).json.copyFrom((__ \ 'name).json.pick) and
              (__ \ 'data \ 'url).json.copyFrom((__ \ 'profile).json.pick)
          }
          reads.reduce
        }

        jsonResult match {
          case JsSuccess(json, _) =>
            Ok(json)
          case JsError(error) =>
            InternalServerError("JSON was malformed: " + error.toString)
        }
      } recoverWith ForceUtils.standardErrorHandler(auth)
    }
  }

}
