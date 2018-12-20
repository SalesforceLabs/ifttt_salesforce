/*
 * Copyright (c) 2018, Salesforce.com, Inc.
 * All rights reserved.
 * SPDX-License-Identifier: BSD-3-Clause
 * For full license text, see the LICENSE.txt file in the repo root or https://opensource.org/licenses/BSD-3-Clause
 */

package controllers.ifttt.v1

import com.github.t3hnar.bcrypt._
import javax.inject.Inject
import play.api.cache.SyncCacheApi
import play.api.libs.json.Reads._
import play.api.libs.json._
import play.api.mvc.{InjectedController, Result}
import utils.{Force, ForceIFTTT}

import scala.concurrent.{ExecutionContext, Future}

class Triggers @Inject()(force: Force, forceIFTTT: ForceIFTTT, cache: SyncCacheApi)(implicit ec: ExecutionContext) extends InjectedController {

  private def unauthorized(error: String): Result = {
    Unauthorized(
      Json.obj(
        "errors" -> Json.arr(
          Json.obj(
            "message" -> error
          )
        )
      )
    )
  }

  def opportunityWasWon = Action.async(parse.json) { request =>

    val limit = (request.body \ "limit").asOpt[Int].getOrElse(5000)

    request.headers.get(AUTHORIZATION).map { auth =>
      forceIFTTT.opportunitiesWon(auth, limit).map(Ok(_)).recoverWith(force.standardErrorHandler(auth))
    } getOrElse Future.successful(unauthorized("Authorization Header Not Set"))
  }

  def customSalesforceTrigger() = Action.async(parse.json) { request =>

    val limit = (request.body \ "limit").asOpt[Int].getOrElse(5000)

    val maybeEventType = (request.body \ "triggerFields" \ "type").asOpt[String]

    maybeEventType.fold {
      val json = Json.obj(
        "errors" -> Json.arr(
          Json.obj(
            "message" -> "trigger field 'type' is required"
          )
        )
      )
      Future.successful(BadRequest(json))
    } { eventType =>
      request.headers.get(AUTHORIZATION).map { auth =>
        force.describe(auth, "ifttt__IFTTT_Event__c").flatMap { describeJson =>

          val fields = (describeJson \ "fields").as[Seq[JsObject]].map(_.\("name").as[String])

          val whereStatement = if (eventType != "") {
            s"WHERE ifttt__Type__c = '$eventType'"
          }
          else {
            ""
          }

          val query = s"""
            |SELECT ${fields.mkString(", ")}
            |FROM ifttt__IFTTT_Event__c
            |$whereStatement

            |ORDER BY LastModifiedDate DESC
            |LIMIT $limit
          """.stripMargin

          forceIFTTT.iftttEventQuery(auth, query).map(Ok(_))
        } recoverWith force.standardErrorHandler(auth)
      } getOrElse Future.successful(unauthorized("Authorization Header Not Set"))
    }
  }

  def recordCreatedOrUpdatedTrigger() = Action.async(parse.json) { request =>

    val limit = (request.body \ "limit").asOpt[Int].getOrElse(5000)

    val maybeSObjectAndQueryCritera = for {
      sobject <- (request.body \ "triggerFields" \ "sobject").asOpt[String]
      queryCriteria <- (request.body \ "triggerFields" \ "query_criteria").asOpt[String]
    } yield (sobject, queryCriteria)

    maybeSObjectAndQueryCritera.fold {
      val json = Json.obj(
        "errors" -> Json.arr(
          Json.obj(
            "message" -> "trigger fields 'sobject' and 'query_criteria' are required"
          )
        )
      )
      Future.successful(BadRequest(json))
    } { case (sobject, queryCriteria) =>

      request.headers.get(AUTHORIZATION).map { auth =>
        val authHash = auth.bcrypt

        val timeStampFieldCacheKey = s"$authHash-$sobject-timestampfield"

        val maybeTimeStampFieldCache = cache.get[String](timeStampFieldCacheKey)

        val timeStampFieldFuture = maybeTimeStampFieldCache.map(Future.successful).getOrElse {
          force.describe(auth, sobject).flatMap { describeJson =>
            val fields = (describeJson \ "fields").as[Seq[JsObject]].map(_.\("name").as[String])
            val maybeTimeStampField = fields.find { s =>
              s == "LastModifiedDate" || s == "SystemModstamp"
            }

            maybeTimeStampField.fold {
              Future.failed[String](Force.ForceError(s"Could not find a record time stamp field on $sobject"))
            } { timeStampField =>
              cache.set(timeStampFieldCacheKey, timeStampField)
              Future.successful(timeStampField)
            }
          }
        }

        timeStampFieldFuture.flatMap { timeStampField =>
          val whereStatement = if (queryCriteria != "") {
            s"WHERE $queryCriteria"
          }
          else {
            ""
          }

          val query = s"""
                         |SELECT Id, $timeStampField
                         |FROM $sobject
                         |$whereStatement
                         |ORDER BY $timeStampField DESC
                         |LIMIT $limit
            """.stripMargin

          forceIFTTT.query(auth, query).map(Ok(_))
        } recoverWith force.standardErrorHandler(auth)
      } getOrElse Future.successful(unauthorized("Authorization Header Not Set"))
    }
  }

  def recordCreatedOrUpdatedTriggerFieldsSObjectOptions() = force.sobjectOptions("queryable")

}
