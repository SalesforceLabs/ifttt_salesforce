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
                "sobject" -> "Contact",
                "field_name_1" -> "FirstName",
                "field_value_1" -> "Foo",
                "field_name_2" -> "LastName",
                "field_value_2" -> "Bar",
                "field_name_3" -> "Email",
                "field_value_3" -> "foo@bar.com",
                "field_name_4" -> "Phone",
                "field_value_4" -> "303-555-1212",
                "field_name_5" -> "Title",
                "field_value_5" -> "Chief"
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