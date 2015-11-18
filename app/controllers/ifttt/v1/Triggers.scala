package controllers.ifttt.v1

import java.text.NumberFormat

import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import play.api.libs.ws.WS
import play.api.mvc.{Controller, Action}
import play.api.Play.current
import play.api.libs.json._
import play.api.libs.json.Reads._
import utils.ForceUtils
import utils.ForceUtils.ForceError
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future

object Triggers extends Controller {

  // 2015-07-06T19:07:56.000+0000
  val jodaDateTimeReads = jodaDateReads("yyyy-MM-dd'T'HH:mm:ss.SSSZZ")

  // todo: I'm sure there is a much better way to compose these JSON transformers

  def opportunityWonQueryResultToIFTTT(instanceUrl: String, amounts: Map[String, Int]): Reads[JsObject] = (__ \ 'data).json.copyFrom(
    (__ \ 'records).json.pick(
      of[JsArray].map {
        case JsArray(arr) =>
          val newArr = arr.map {
            case j: JsObject =>
              val id = (j \ "Id").as[String]
              val timestamp = (j \ "LastModifiedDate").as[DateTime](jodaDateTimeReads)
              val name = (j \ "Name").as[String]
              val oppId = (j \ "ifttt__Related_Object_Id__c").as[String]

              val nf = NumberFormat.getCurrencyInstance
              nf.setMaximumFractionDigits(0)

              Json.obj(
                "name" -> name,
                "amount" -> nf.format(amounts.getOrElse(oppId, 0)),
                "link_to_opportunity" -> (instanceUrl + oppId),
                "timestamp" -> ISODateTimeFormat.dateTime().print(timestamp),
                "meta" -> Json.obj(
                  "id" -> id,
                  "timestamp" -> timestamp.getMillis / 1000
                )
              )
            case _ =>
              Json.obj()
          }
          JsArray(newArr)
      }
    )
  )

  val iftttEventQueryResultToIFTTT: Reads[JsObject] = (__ \ 'data).json.copyFrom(
    (__ \ 'records).json.pick(
      of[JsArray].map {
        case JsArray(arr) =>
          val newArr = arr.map {
            case j: JsObject =>
              val id = (j \ "Id").as[String]
              val timestamp = (j \ "LastModifiedDate").as[DateTime](jodaDateTimeReads)
              val subject = (j \ "Name").as[String]
              val eventType = (j \ "ifttt__Type__c").asOpt[String].getOrElse("")
              val message = (j \ "ifttt__Message__c").asOpt[String].getOrElse("")

              Json.obj(
                "type" -> eventType,
                "subject" -> subject,
                "message" -> message,
                "timestamp" -> ISODateTimeFormat.dateTime().print(timestamp),
                "meta" -> Json.obj(
                  "id" -> id,
                  "timestamp" -> timestamp.getMillis / 1000
                )
              )
            case _ =>
              Json.obj()
          }
          JsArray(newArr)
      }
    )
  )

  def anyQueryResultToIFTTT(instanceUrl: String): Reads[JsObject] = (__ \ 'data).json.copyFrom(
    (__ \ 'records).json.pick(
      of[JsArray].map {
        case JsArray(arr) =>
          val newArr = arr.map {
            case j: JsObject =>
              val id = (j \ "Id").as[String]
              val timestamp = (j \ "LastModifiedDate").as[DateTime](jodaDateTimeReads)
              val linkToRecord = instanceUrl + id

              Json.obj(
                "link_to_record" -> linkToRecord,
                "timestamp" -> ISODateTimeFormat.dateTime().print(timestamp),
                "meta" -> Json.obj(
                  "id" -> id,
                  "timestamp" -> timestamp.getMillis / 1000
                )
              )
            case _ =>
              Json.obj()
          }
          JsArray(newArr)
      }
    )
  )


  def opportunityWasWon = Action.async(parse.json) { request =>

    val limit = (request.body \ "limit").asOpt[Int].getOrElse(5000)

    request.headers.get(AUTHORIZATION).map { auth =>

      ForceUtils.userinfo(auth).flatMap { userinfo =>

            val queryUrl = ForceUtils.queryUrl(userinfo)

            val sobjectUrl = ForceUtils.sobjectsUrl(userinfo)

            //val userId = (userinfoResponse.json \ "user_id").as[String]

            //val orgId = (userinfoResponse.json \ "organization_id").as[String]

            val instanceUrl = ForceUtils.instanceUrl(userinfo)

            val query = s"""
                |SELECT Id, LastModifiedDate, Name, ifttt__Type__c, ifttt__Message__c, ifttt__Related_Object_Type__c, ifttt__Related_Object_Id__c
                |FROM ifttt__IFTTT_Event__c
                |WHERE ifttt__Type__c = 'Opportunity Won'
                |ORDER BY LastModifiedDate DESC
                |LIMIT $limit
              """.stripMargin

            val queryRequest = WS.url(queryUrl).withHeaders(AUTHORIZATION -> auth).withQueryString("q" -> query).get().flatMap { response =>
              response.status match {
                case OK =>
                  Future.successful(response)
                case _ =>
                  Future.failed(ForceError(response.json))
              }
            }

            queryRequest.flatMap { queryResponse =>

              val amountsFutures = (queryResponse.json \ "records").as[Seq[JsObject]].map { record =>
                val oppId = (record \ "ifttt__Related_Object_Id__c").as[String]
                val oppType = (record \ "ifttt__Related_Object_Type__c").as[String]
                val url = s"$sobjectUrl$oppType/$oppId"

                WS.url(url).withHeaders(AUTHORIZATION -> auth).get().map { opp =>
                  (oppId, (opp.json \ "Amount").asOpt[Int])
                }
              }

              Future.sequence(amountsFutures).flatMap { amountsSeq =>
                val amounts = amountsSeq.filter(_._2.isDefined).toMap.mapValues(_.get)

                queryResponse.json.transform(opportunityWonQueryResultToIFTTT(instanceUrl, amounts)).fold(
                  { _ =>
                    Future.failed(ForceError(queryResponse.json))
                  },
                  { resultJson =>
                    Future.successful(Ok(resultJson))
                  }
                )
              }
            }
        } recoverWith ForceUtils.standardErrorHandler(auth)

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

        ForceUtils.userinfo(auth).flatMap { userinfo =>
          val url = ForceUtils.queryUrl(userinfo)

          //val userId = (userinfoResponse.json \ "user_id").as[String]

          //val orgId = (userinfoResponse.json \ "organization_id").as[String]

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

          val queryRequest = WS.url(url).withHeaders(AUTHORIZATION -> auth).withQueryString("q" -> query).get().flatMap { response =>
            response.status match {
              case OK =>
                Future.successful(response)
              case _ =>
                Future.failed(new Exception(response.body))
            }
          }

          queryRequest.flatMap { queryResponse =>
            queryResponse.json.transform(iftttEventQueryResultToIFTTT).fold(
              { _ =>
                Future.failed(ForceError(queryResponse.json))
              },
              { json =>
                Future.successful(Ok(json))
              }
            )
          }
        } recoverWith ForceUtils.standardErrorHandler(auth)
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

        ForceUtils.userinfo(auth).flatMap { userinfo =>
          val url = ForceUtils.queryUrl(userinfo)

          val instanceUrl = ForceUtils.instanceUrl(userinfo)

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

          val queryRequest = WS.url(url).withHeaders(AUTHORIZATION -> auth).withQueryString("q" -> query).get()

          queryRequest.flatMap { queryResponse =>
            queryResponse.status match {
              case OK =>
                queryResponse.json.transform(anyQueryResultToIFTTT(instanceUrl)).fold(
                  { _ =>
                    Future.failed(ForceError(queryResponse.json))
                  },
                  { json =>
                    Future.successful(Ok(json))
                  }
                )
              case _ =>
                Future.failed(ForceError(queryResponse.json))
            }
          }
        } recoverWith ForceUtils.standardErrorHandler(auth)
      } getOrElse Future.successful(Unauthorized)
    }
  }

  def recordCreatedOrUpdatedTriggerFieldsSObjectOptions() = ForceUtils.sobjectOptions("queryable")

}