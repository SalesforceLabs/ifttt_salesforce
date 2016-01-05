package controllers.ifttt.v1

import play.api.libs.json.Reads._
import play.api.libs.json._
import play.api.mvc.{Action, Controller}
import utils.{ForceIFTTT, Adapters, Force}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Actions extends Controller {

  private def maybeCommunityGroup(maybeValue: Option[String]): (Option[String], Option[String]) = {
    maybeValue.fold(Option.empty[String], Option.empty[String]) { value =>
      value.split(':') match {
        case Array(communityId, groupId) => (Some(communityId), Some(groupId))
        case Array(groupId) => (None, Some(groupId))
        case _ => (None, None)
      }
    }
  }

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
      val (maybeCommunityId, maybeGroupId) = maybeCommunityGroup((request.body \ "actionFields" \ "group").asOpt[String])

      maybeMessage.fold(Future.successful(BadRequest(error("MISSING_REQUIRED_FIELD", "Message field was missing")))) { message =>
        Force.chatterPostMessage(auth, message, maybeCommunityId, maybeGroupId).map((responseJson _).tupled).map(Ok(_))
      } recoverWith Force.standardErrorHandler(auth)
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
      val (maybeCommunityId, maybeGroupId) = maybeCommunityGroup((request.body \ "actionFields" \ "group").asOpt[String])
      val maybeFileUrl = (request.body \ "actionFields" \ "file_url").asOpt[String]
      val maybeFileName = (request.body \ "actionFields" \ "file_name").asOpt[String]

      val resultFuture = (maybeFileUrl, maybeFileName) match {
        case (Some(fileUrl), Some(fileName)) =>
          Force.chatterPostFile(auth, fileUrl, fileName, maybeMessage, maybeCommunityId, maybeGroupId).map((responseJson _).tupled).map(Ok(_))
        case _ =>
          Future.successful(BadRequest(error("MISSING_REQUIRED_FIELD", "Both the File URL and File Name are required")))
      }

      resultFuture.recoverWith(Force.standardErrorHandler(auth))
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
      val (maybeCommunityId, maybeGroupId) = maybeCommunityGroup((request.body \ "actionFields" \ "group").asOpt[String])
      val maybeLinkUrl = (request.body \ "actionFields" \ "link").asOpt[String]

      maybeLinkUrl.fold(Future.successful(BadRequest(error("MISSING_REQUIRED_FIELD", "The link is required")))) { linkUrl =>
        Force.chatterPostLink(auth, linkUrl, maybeMessage, maybeCommunityId, maybeGroupId).map((responseJson _).tupled).map(Ok(_))
      } recoverWith Force.standardErrorHandler(auth)
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

        Force.insert(auth, sobject, jsonToInsert).map { createJson =>
          val id = (createJson \ "id").as[String]

          val json = Json.obj(
            "data" -> Json.arr(
              Json.obj(
                "id" -> id
              )
            )
          )

          Ok(json)
        } recoverWith Force.standardErrorHandler(auth)
      }
    }
  }

  def insertARecordFieldsSObjectOptions() = Force.sobjectOptions("createable")

  def postOnChatterFieldsGroupOptions() = Action.async(parse.json) { request =>
    request.headers.get(AUTHORIZATION).fold(Future.successful(Unauthorized(""))) { auth =>
      ForceIFTTT.allGroups(auth).map(Ok(_)).recoverWith(Force.standardErrorHandler(auth))
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
