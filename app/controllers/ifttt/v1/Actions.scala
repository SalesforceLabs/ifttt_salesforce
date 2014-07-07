package controllers.ifttt.v1

import play.api.libs.json.{JsString, JsObject, Json}
import play.api.mvc.{Action, Controller}
import utils.ForceUtils

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Actions extends Controller {

  def postOnChatter = Action.async(parse.json) { request =>

    request.headers.get(AUTHORIZATION).fold(Future.successful(Unauthorized(""))) { auth =>

      val maybeMessage = (request.body \ "actionFields" \ "message").asOpt[String]

      maybeMessage.fold(Future.successful(BadRequest(error("MISSING_REQUIRED_FIELD", "Message field was missing")))) { message =>

        ForceUtils.chatterPost(auth, message).map {
          case (response, Some(instanceUrl)) if response.status == CREATED =>

            val id = (response.json \ "id").as[String]

            val json = Json.obj(
              "data" -> Json.arr(
                Json.obj(
                  "id" -> id,
                  "url" -> (instanceUrl + id)
                )
              )
            )

            Ok(json)

          case (response, None) if response.status == FORBIDDEN =>
            Unauthorized(error("Unauthorized", response.body))

          case (response, _) =>
            Status(response.status)(response.body)
        }
      }
    }
  }

  private def error(status: String, message: String): JsObject = {
    Json.obj(
      "errors" -> Json.arr(
        Json.obj(
          "status" -> status,
          "message" -> message
        )
      )
    )
  }

}
