package controllers.ifttt.v1

import play.api.libs.json.Reads._
import play.api.libs.json._
import play.api.mvc.{Action, Controller, Result}
import utils.Force.UnauthorizedException
import utils.{Force, ForceIFTTT}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Triggers extends Controller {

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
      ForceIFTTT.opportunitiesWon(auth, limit).map(Ok(_)).recoverWith(Force.standardErrorHandler(auth))
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
        Force.describe(auth, "ifttt__IFTTT_Event__c").flatMap { describeJson =>

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

          ForceIFTTT.iftttEventQuery(auth, query).map(Ok(_)).recoverWith(Force.standardErrorHandler(auth))
        } recover {
          case ua: UnauthorizedException => unauthorized(ua.message)
        }
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

        val whereStatement = if (queryCriteria != "") {
          s"WHERE $queryCriteria"
        }
        else {
          ""
        }

        val query = s"""
          |SELECT Id, LastModifiedDate
          |FROM $sobject
          |$whereStatement
          |ORDER BY LastModifiedDate DESC
          |LIMIT $limit
        """.stripMargin

        ForceIFTTT.query(auth, query).map(Ok(_)).recoverWith(Force.standardErrorHandler(auth))
      } getOrElse Future.successful(unauthorized("Authorization Header Not Set"))
    }
  }

  def recordCreatedOrUpdatedTriggerFieldsSObjectOptions() = Force.sobjectOptions("queryable")

}