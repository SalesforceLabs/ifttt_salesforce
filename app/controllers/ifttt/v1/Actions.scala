package controllers.ifttt.v1

import play.api.libs.json.Reads._
import play.api.libs.json._
import play.api.mvc.{Action, Controller}
import utils.{Adapters, ForceUtils}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Actions extends Controller {

  def postMessageOnChatter = Action.async(parse.json) { request =>
    request.headers.get(AUTHORIZATION).fold(Future.successful(Unauthorized(""))) { auth =>

      def responseJson(json: JsValue, instanceUrl: String): JsValue = {
        val id = (json \ "id").as[String]

        Json.obj(
          "data" -> Json.arr(
            Json.obj(
              "id" -> id,
              "url" -> (instanceUrl + id)
            )
          )
        )
      }

      val maybeMessage = (request.body \ "actionFields" \ "message").asOpt[String].filterNot(_.isEmpty)
      val maybeGroup = (request.body \ "actionFields" \ "group").asOpt[String].filterNot(_.isEmpty)

      maybeMessage.fold(Future.successful(BadRequest(error("MISSING_REQUIRED_FIELD", "Message field was missing")))) { message =>
        ForceUtils.chatterPostMessage(auth, message, maybeGroup).map((responseJson _).tupled).map(Ok(_))
      } recoverWith ForceUtils.standardErrorHandler(auth)
    }
  }

  def postFileOnChatter = Action.async(parse.json) { request =>
    request.headers.get(AUTHORIZATION).fold(Future.successful(Unauthorized(""))) { auth =>

      def responseJson(json: JsValue, instanceUrl: String): JsValue = {
        val id = (json \ "id").as[String]

        Json.obj(
          "data" -> Json.arr(
            Json.obj(
              "id" -> id,
              "url" -> (instanceUrl + id)
            )
          )
        )
      }

      val maybeMessage = (request.body \ "actionFields" \ "message").asOpt[String].filterNot(_.isEmpty)
      val maybeGroup = (request.body \ "actionFields" \ "group").asOpt[String].filterNot(_.isEmpty)
      val maybeFileUrl = (request.body \ "actionFields" \ "file_url").asOpt[String]
      val maybeFileName = (request.body \ "actionFields" \ "file_name").asOpt[String]

      val resultFuture = (maybeFileUrl, maybeFileName) match {
        case (Some(fileUrl), Some(fileName)) =>
          ForceUtils.chatterPostFile(auth, fileUrl, fileName, maybeMessage, maybeGroup).map((responseJson _).tupled).map(Ok(_))
        case _ =>
          Future.successful(BadRequest(error("MISSING_REQUIRED_FIELD", "Both the File URL and File Name are required")))
      }

      resultFuture.recoverWith(ForceUtils.standardErrorHandler(auth))
    }
  }

  def postLinkOnChatter = Action.async(parse.json) { request =>

    request.headers.get(AUTHORIZATION).fold(Future.successful(Unauthorized(""))) { auth =>

      def responseJson(json: JsValue, instanceUrl: String): JsValue = {
        val id = (json \ "id").as[String]

        Json.obj(
          "data" -> Json.arr(
            Json.obj(
              "id" -> id,
              "url" -> (instanceUrl + id)
            )
          )
        )
      }

      val maybeMessage = (request.body \ "actionFields" \ "message").asOpt[String].filterNot(_.isEmpty)
      val maybeGroup = (request.body \ "actionFields" \ "group").asOpt[String].filterNot(_.isEmpty)
      val maybeLinkUrl = (request.body \ "actionFields" \ "link").asOpt[String]

      maybeLinkUrl.fold(Future.successful(BadRequest(error("MISSING_REQUIRED_FIELD", "The link is required")))) { linkUrl =>
        ForceUtils.chatterPostLink(auth, linkUrl, maybeMessage, maybeGroup).map((responseJson _).tupled).map(Ok(_))
      } recoverWith ForceUtils.standardErrorHandler(auth)
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
