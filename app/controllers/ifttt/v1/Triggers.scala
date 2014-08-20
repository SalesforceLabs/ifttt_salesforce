package controllers.ifttt.v1

import java.text.NumberFormat
import java.util.Date

import play.api.libs.ws.WS
import play.api.mvc.{Controller, Action}
import play.api.Play.current
import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._
import utils.ForceUtils
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future

object Triggers extends Controller {

  val opportunityQueryResultToIFTTT: Reads[JsObject] = (__ \ 'data).json.copyFrom(
    (__ \ 'records).json.pick(
      of[JsArray].map {
        case JsArray(arr) =>
          val newArr = arr.map {
            case j: JsObject =>
              val name = (j \ "Name").as[String]
              val amount = (j \ "Amount").asOpt[Int].getOrElse(0)
              val id = (j \ "Id").as[String]
              val timestamp = (j \ "LastModifiedDate").as[Date]

              val nf = NumberFormat.getCurrencyInstance
              nf.setMaximumFractionDigits(0)

              Json.obj(
                "name" -> name,
                "amount" -> nf.format(amount),
                "meta" -> Json.obj(
                  "id" -> id,
                  "timestamp" -> timestamp.getTime / 1000
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
              val timestamp = (j \ "LastModifiedDate").as[Date]
              val subject = (j \ "Name").as[String]
              val eventType = (j \ "ifttt__Type__c").asOpt[String].getOrElse("")
              val message = (j \ "ifttt__Message__c").asOpt[String].getOrElse("")

              Json.obj(
                "type" -> eventType,
                "subject" -> subject,
                "message" -> message,
                "meta" -> Json.obj(
                  "id" -> id,
                  "timestamp" -> timestamp.getTime / 1000
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

      ForceUtils.userinfo(auth).flatMap { userinfoResponse =>

        userinfoResponse.status match {
          case OK =>
            val url = (userinfoResponse.json \ "urls" \ "query").as[String].replace("{version}", "30.0")

            val userId = (userinfoResponse.json \ "user_id").as[String]

            val query =
              s"""SELECT Id, Name, Amount, LastModifiedDate
             |FROM Opportunity
             |WHERE OwnerId = '$userId' AND StageName = 'Closed Won'
             |ORDER BY LastModifiedDate DESC
             |LIMIT $limit
           """.stripMargin

            val request = WS.url(url).withHeaders(AUTHORIZATION -> auth).withQueryString("q" -> query).get()

            request.map { queryResponse =>

              val jsonResult = queryResponse.json.transform(opportunityQueryResultToIFTTT)

              jsonResult match {
                case JsSuccess(json, _) =>
                  Ok(json)
                case JsError(error) =>
                  InternalServerError(Json.obj("error" -> error.toString))
              }
            }
          case FORBIDDEN =>
            val json = Json.obj(
              "errors" -> Json.arr(
                Json.obj(
                  "status" -> userinfoResponse.body,
                  "message" -> ("Authentication failed: " + userinfoResponse.body)
                )
              )
            )
            Future.successful(Unauthorized(json))
          case _ =>
            Future.successful(Status(userinfoResponse.status)(userinfoResponse.body))
        }
      }

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

        ForceUtils.userinfo(auth).flatMap { userinfoResponse =>

          userinfoResponse.status match {
            case OK =>
              val url = (userinfoResponse.json \ "urls" \ "query").as[String].replace("{version}", "30.0")

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

              val queryRequest = WS.url(url).withHeaders(AUTHORIZATION -> auth).withQueryString("q" -> query).get()

              queryRequest.map { queryResponse =>

                val jsonResult = queryResponse.json.transform(iftttEventQueryResultToIFTTT)

                jsonResult match {
                  case JsSuccess(json, _) =>
                    Ok(json)
                  case JsError(error) =>
                    InternalServerError(Json.obj("error" -> error.toString))
                }
              }
            case FORBIDDEN =>
              val json = Json.obj(
                "errors" -> Json.arr(
                  Json.obj(
                    "status" -> userinfoResponse.body,
                    "message" -> ("Authentication failed: " + userinfoResponse.body)
                  )
                )
              )
              Future.successful(Unauthorized(json))
            case _ =>
              Future.successful(Status(userinfoResponse.status)(userinfoResponse.body))
          }
        }

      } getOrElse Future.successful(Unauthorized)
    }

  }

}