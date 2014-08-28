package controllers.ifttt.v1

import play.api.libs.ws.WS
import play.api.mvc.{Action, Controller}
import play.api.Play.current
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import utils.{Global, ForceUtils}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object User extends Controller {

  def info = Action.async { implicit request =>
    request.headers.get(AUTHORIZATION) match {
      case Some(auth) =>
        val userinfoFuture = ForceUtils.userinfo(auth)

        userinfoFuture.map { userinfoResponse =>
          userinfoResponse.status match {
            case OK =>

              val authToken = auth.stripPrefix("Bearer ")

              val instanceUrl = ForceUtils.instanceUrl(userinfoResponse.json)

              val userId = (userinfoResponse.json \ "user_id").as[String]

              val orgId = (userinfoResponse.json \ "organization_id").as[String]

              // add the user to the watchers in this org in order to support real-time notifications
              Global.redis.sadd(orgId, userId)

              val jsonResult = userinfoResponse.json.transform {
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
            case FORBIDDEN =>
              val json = Json.obj(
                "errors" -> Json.arr(
                  Json.obj(
                    "status" -> userinfoResponse.body,
                    "message" -> ("Authentication failed: " + userinfoResponse.body)
                  )
                )
              )
              Unauthorized(json)
            case _ =>
              Status(userinfoResponse.status)(userinfoResponse.body)
          }

        }
      case None =>
        Future.successful(Unauthorized("Request did not contain an Authorization header"))
    }
  }

}
