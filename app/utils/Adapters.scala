package utils

import java.text.NumberFormat

import org.joda.time.DateTime
import org.joda.time.format.ISODateTimeFormat
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

object Adapters {

  def anyJsValueToSalesforce(jsValue: JsValue): JsValue = {

    // July 10, 2015 at 8:00AM
    val googleDateTimeReads: Reads[JsValue] = jodaDateReads("MMMM dd, YYYY 'at' hh:mma").map { dateTime =>
      JsNumber(dateTime.getMillis)
    }

    val booleanStringReads: Reads[JsValue] = StringReads.filter { s =>
      s.equalsIgnoreCase("true") || s.equalsIgnoreCase("false")
    } map { s =>
      JsBoolean(s.toBoolean)
    }

    val allReads: Reads[JsValue] = googleDateTimeReads or booleanStringReads

    val maybeAdapted: JsResult[JsValue] = jsValue.transform(allReads)

    maybeAdapted.getOrElse(jsValue)
  }

  // 2015-07-06T19:07:56.000+0000
  val jodaDateTimeReads = jodaDateReads("yyyy-MM-dd'T'HH:mm:ss.SSSZZ")

  // todo: I'm sure there is a much better way to compose these JSON transformers

  val nf = NumberFormat.getCurrencyInstance
  nf.setMaximumFractionDigits(0)

  def opportunityWonQueryResultToIFTTT(instanceUrl: String): Reads[JsObject] = (__ \ 'data).json.copyFrom(
    (__ \ 'records).json.pick(
      of[JsArray].map {
        case JsArray(arr) =>
          val newArr = arr.map {
            case j: JsObject =>
              val oppId = (j \ "Id").as[String]
              val timestamp = (j \ "LastModifiedDate").as[DateTime](jodaDateTimeReads)
              val name = (j \ "Name").as[String]
              val amount = nf.format((j \ "Amount").asOpt[Double].getOrElse(0))
              val ownerName = (j \ "Owner" \ "Name").as[String]

              Json.obj(
                "name" -> name,
                "amount" -> amount,
                "link_to_opportunity" -> (instanceUrl + oppId),
                "timestamp" -> ISODateTimeFormat.dateTime().print(timestamp),
                "owner_name" -> ownerName,
                "meta" -> Json.obj(
                  "id" -> oppId,
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

}
