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
                "sobject" -> "Task",
                "query_criteria" -> "Subject != NULL"
              )
            ),
            "actions" -> Json.obj(
              "post_on_chatter" -> Json.obj(
                "message" -> "A test message"
              ),
              "insert_a_record" -> Json.obj(
                "sobject" -> "Event",
                "field_name_1" -> "StartDateTime",
                "field_value_1" -> "July 10, 2015 at 8:00AM",
                "field_name_2" -> "EndDateTime",
                "field_value_2" -> "July 10, 2015 at 10:00AM",
                "field_name_3" -> "Description",
                "field_value_3" -> "Test Event",
                "field_name_4" -> "IsPrivate",
                "field_value_4" -> "false",
                "field_name_5" -> "Subject",
                "field_value_5" -> "Call"
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