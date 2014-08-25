package controllers.ifttt.v1

import controllers.ifttt.v1.Application._
import play.api.Play
import play.api.libs.json.Json
import play.api.mvc.Action

object Test {

  def setup = Action { request =>
    val maybeOk = for {
      received <- request.headers.get("IFTTT-Channel-Key")
      expected <- Play.current.configuration.getString("ifttt.channel.key")
      if received == expected
    } yield {
      val json = Json.obj(
        "data" -> Json.obj(
          "accessToken" -> Play.current.configuration.getString("ifttt.test.accesstoken").get,
          "samples" -> Json.obj(
            "triggers" -> Json.obj(
              "custom_salesforce_trigger" -> Json.obj(
                "type" -> "test"
              ),
              "record_created_or_updated_trigger" -> Json.obj(
                "sobject" -> "Contact",
                "query_criteria" -> "Phone != null"
              )
            ),
            "actions" -> Json.obj(
              "post_on_chatter" -> Json.obj(
                "message" -> "A test message"
              )
            )
          )
        )
      )

      Ok(json)
    }

    maybeOk.getOrElse(Unauthorized)
  }

}