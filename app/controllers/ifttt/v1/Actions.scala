/*
 * Copyright (c) 2018, Salesforce.com, Inc.
 * All rights reserved.
 * SPDX-License-Identifier: BSD-3-Clause
 * For full license text, see the LICENSE.txt file in the repo root or https://opensource.org/licenses/BSD-3-Clause
 */

package controllers.ifttt.v1

import javax.inject.{Inject, Singleton}
import play.api.libs.json.Reads._
import play.api.libs.json._
import play.api.mvc.InjectedController
import utils.{Adapters, Force, ForceIFTTT}

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class Actions @Inject() (force: Force, forceIFTTT: ForceIFTTT) (implicit ec: ExecutionContext) extends InjectedController {

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
        force.chatterPostMessage(auth, message, maybeCommunityId, maybeGroupId).map((responseJson _).tupled).map(Ok(_))
      } recoverWith force.standardErrorHandler(auth)
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
          force.chatterPostFile(auth, fileUrl, fileName, maybeMessage, maybeCommunityId, maybeGroupId).map((responseJson _).tupled).map(Ok(_))
        case _ =>
          Future.successful(BadRequest(error("MISSING_REQUIRED_FIELD", "Both the File URL and File Name are required")))
      }

      resultFuture.recoverWith(force.standardErrorHandler(auth))
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
        force.chatterPostLink(auth, linkUrl, maybeMessage, maybeCommunityId, maybeGroupId).map((responseJson _).tupled).map(Ok(_))
      } recoverWith force.standardErrorHandler(auth)
    }
  }

  def insertARecord() = Action.async(parse.json) { request =>

    request.headers.get(AUTHORIZATION).fold(Future.successful(Unauthorized(""))) { auth =>

      val maybeSobject = (request.body \ "actionFields" \ "sobject").asOpt[String]

      def maybeNameValue(num: Int): Option[(String, JsValue)] = {
        (request.body \ "actionFields" \ s"field_name_$num").asOpt[String].filter(_.length > 0).map { fieldName =>
          fieldName -> Adapters.anyJsValueToSalesforce((request.body \ "actionFields" \ s"field_value_$num").as[JsValue])
        }
      }

      val jsonToInsert = JsObject((1 to 5).flatMap(maybeNameValue))

      maybeSobject.fold(Future.successful(BadRequest(error("MISSING_REQUIRED_FIELD", "An SObject must be specified")))) { sobject =>

        force.insert(auth, sobject, jsonToInsert).map { createJson =>
          val id = (createJson \ "id").as[String]

          val json = Json.obj(
            "data" -> Json.arr(
              Json.obj(
                "id" -> id
              )
            )
          )

          Ok(json)
        } recoverWith force.standardErrorHandler(auth)
      }
    }
  }

  def insertARecordFieldsSObjectOptions() = force.sobjectOptions("createable")

  def postOnChatterFieldsGroupOptions() = Action.async(parse.json) { request =>
    request.headers.get(AUTHORIZATION).fold(Future.successful(Unauthorized(""))) { auth =>
      forceIFTTT.allGroups(auth).map(Ok(_)).recoverWith(force.standardErrorHandler(auth))
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
