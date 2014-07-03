package controllers.ifttt.v1

import play.api.Play
import play.api.libs.json.Json
import play.api.libs.ws.WS
import play.api.mvc.{Action, Controller}
import play.api.Play.current
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future

object Webhooks extends Controller {

  def opportunityWasWon() = Action.async(parse.json) { request =>

    val ownerId = (request.body \ "new" \\ "OwnerId").seq.head.as[String]

    val oldStage = (request.body \ "old" \\ "StageName").seq.head.as[String]
    val newStage = (request.body \ "new" \\ "StageName").seq.head.as[String]

    if ((newStage != oldStage) && (newStage == "Closed Won")) {

      WS.
        url("https://realtime.ifttt.com/v1/notifications").
        withHeaders(
          "IFTTT-Channel-Key" -> Play.current.configuration.getString("ifttt.channel.key").get
        ).
        post(
          Json.obj(
            "data" -> Json.arr(
              Json.obj(
                "user_id" -> ownerId
              )
            )
          )
        ).
        map(r => Ok)
    }
    else {
      Future.successful(Ok)
    }
  }

}