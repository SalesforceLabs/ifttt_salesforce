package controllers.ifttt.v1

import play.api.libs.json.Reads._
import play.api.libs.json._
import play.api.mvc.{Action, Controller}
import utils.{Adapters, ForceUtils}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Actions extends Controller {

  def postOnChatter = Action.async(parse.json) { request =>

    request.headers.get(AUTHORIZATION).fold(Future.successful(Unauthorized(""))) { auth =>

      val maybeMessage = (request.body \ "actionFields" \ "message").asOpt[String]
      val maybeGroup = (request.body \ "actionFields" \ "group").asOpt[String]

      maybeMessage.fold(Future.successful(BadRequest(error("MISSING_REQUIRED_FIELD", "Message field was missing")))) { message =>

        ForceUtils.chatterPostMessage(auth, message, maybeGroup).map { case (createJson, instanceUrl) =>

            val id = (createJson \ "id").as[String]

            val json = Json.obj(
              "data" -> Json.arr(
                Json.obj(
                  "id" -> id,
                  "url" -> (instanceUrl + id)
                )
              )
            )

            Ok(json)
        } recoverWith ForceUtils.standardErrorHandler(auth)
      }
    }
  }

  def insertARecord() = Action.async(parse.json) { request =>

    request.headers.get(AUTHORIZATION).fold(Future.successful(Unauthorized(""))) { auth =>

      val maybeSobject = (request.body \ "actionFields" \ "sobject").asOpt[String]

      def maybeNameValue(num: Int): Option[(String, JsValue)] = {
        (request.body \ "actionFields" \ s"field_name_$num").asOpt[String].filter(_.length > 0).map { fieldName =>
          fieldName -> Adapters.anyJsValueToSalesforce(request.body \ "actionFields" \ s"field_value_$num")
        }
      }

      val jsonToInsert = JsObject((1 to 5).flatMap(maybeNameValue))

      maybeSobject.fold(Future.successful(BadRequest(error("MISSING_REQUIRED_FIELD", "An SObject must be specified")))) { sobject =>

        ForceUtils.insert(auth, sobject, jsonToInsert).map { createJson =>
          val id = (createJson \ "id").as[String]

          val json = Json.obj(
            "data" -> Json.arr(
              Json.obj(
                "id" -> id
              )
            )
          )

          Ok(json)
        } recoverWith ForceUtils.standardErrorHandler(auth)
      }
    }
  }

  def insertARecordFieldsSObjectOptions() = ForceUtils.sobjectOptions("createable")

  def postOnChatterFieldsGroupOptions() = Action.async(parse.json) { request =>
    request.headers.get(AUTHORIZATION).fold(Future.successful(Unauthorized(""))) { auth =>
      ForceUtils.chatterGroups(auth).map { groups =>

        val options = groups.value.map { group =>
          Json.obj("label" -> (group \ "name").as[String], "value" -> (group \ "id").as[String])
        } sortBy (_.\("label").as[String])

        Ok(
          Json.obj(
            "data" -> options
          )
        )
      }  recoverWith ForceUtils.standardErrorHandler(auth)
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
