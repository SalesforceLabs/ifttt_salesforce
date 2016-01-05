package controllers.ifttt.v1

import play.api.libs.json.Reads._
import play.api.libs.json._
import play.api.mvc.{Action, Controller}
import utils.{ForceIFTTT, Force}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object Triggers extends Controller {

  def opportunityWasWon = Action.async(parse.json) { request =>

    val limit = (request.body \ "limit").asOpt[Int].getOrElse(5000)

    request.headers.get(AUTHORIZATION).map { auth =>
      ForceIFTTT.opportunitiesWon(auth, limit).map(Ok(_)).recoverWith(Force.standardErrorHandler(auth))
    } getOrElse Future.successful(Unauthorized)
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

        val whereStatement = if (eventType != "") {
          s"WHERE ifttt__Type__c = '$eventType'"
        }
        else {
          ""
        }

        val query = s"""
          |SELECT Id, LastModifiedDate, Name, ifttt__Type__c, ifttt__Message__c
          |FROM ifttt__IFTTT_Event__c
          |$whereStatement
          |ORDER BY LastModifiedDate DESC
          |LIMIT $limit
        """.stripMargin

        ForceIFTTT.iftttEventQuery(auth, query).map(Ok(_)).recoverWith(Force.standardErrorHandler(auth))
      } getOrElse Future.successful(Unauthorized)
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
      } getOrElse Future.successful(Unauthorized)
    }
  }

  def recordCreatedOrUpdatedTriggerFieldsSObjectOptions() = Force.sobjectOptions("queryable")

}