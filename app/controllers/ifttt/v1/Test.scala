package controllers.ifttt.v1

import controllers.ifttt.v1.Application._
import play.api.Play
import play.api.libs.json.Json
import play.api.mvc.Action
import utils.Force

import scala.concurrent.ExecutionContext.Implicits.global

object Test {

  def setup = Action.async { request =>

    Force.login(Force.ENV_PROD, Play.current.configuration.getString("ifttt.test.username").get, Play.current.configuration.getString("ifttt.test.password").get).map { loginInfo =>

      val accessToken = (loginInfo \ "access_token").as[String]

      val maybeOk = for {
        received <- request.headers.get("IFTTT-Channel-Key")
        expected <- Play.current.configuration.getString("ifttt.channel.key")
        if received == expected
      } yield {
          val json = Json.obj(
            "data" -> Json.obj(
              "accessToken" -> accessToken,
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
                    "message" -> "A test message",
                    "group" -> "0F9j000000074BA"
                  ),
                  "post_chatter_file" -> Json.obj(
                    "file_url" -> "http://investor.salesforce.com/files/design/newlogo-company.png",
                    "file_name" -> "Foo.png",
                    "message" -> "A test message",
                    "group" -> "0F9j000000074BA"
                  ),
                  "post_chatter_link" -> Json.obj(
                    "link" -> "http://www.jamesward.com",
                    "message" -> "A test message",
                    "group" -> "0F9j000000074BA"
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

}