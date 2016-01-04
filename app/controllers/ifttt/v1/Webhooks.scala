package controllers.ifttt.v1

import java.net.URL

import play.api.Play.current
import play.api.libs.json.Json
import play.api.libs.ws.WS
import play.api.mvc.{Action, Controller}
import utils.Global

import scala.concurrent.ExecutionContext.Implicits.global

object Webhooks extends Controller {

  /*
    An outbound message inserts a new record into the IFTTT Event SObject
   */
  def customSalesforceTrigger() = Action.async(parse.xml) { request =>

    val sessionId = (request.body \ "Body" \ "notifications" \ "SessionId").text

    val partnerUrl = new URL((request.body \ "Body" \ "notifications" \ "PartnerUrl").text)

    val actionId = (request.body \ "Body" \ "notifications" \ "ActionId").text

    val objectType = (request.body \ "Body" \ "notifications" \ "Notification" \ "sObject" \@ "{http://www.w3.org/2001/XMLSchema-instance}type").stripPrefix("sf:")

    val objectId = (request.body \ "Body" \ "notifications" \ "Notification" \ "sObject" \ "Id").text

    val restBaseUrl = s"https://${partnerUrl.getHost}/services/data/v31.0"

    // get the workflow info
    WS.url(s"$restBaseUrl/tooling/sobjects/WorkflowOutboundMessage/$actionId").withHeaders(AUTHORIZATION -> s"Bearer $sessionId").get().flatMap { actionResponse =>

      val actionName = (actionResponse.json \ "Name").as[String]

      // add a new record to ifttt events
      val eventJson = Json.obj(
        "ifttt__Type__c" -> actionName,
        "Name" -> actionName,
        "ifttt__Message__c" -> s"Details: https://${partnerUrl.getHost}/$objectId"
      )

      WS.url(s"$restBaseUrl/sobjects/ifttt__IFTTT_Event__c").withHeaders(AUTHORIZATION -> s"Bearer $sessionId").post(eventJson).map { createEventResponse =>

        val success = (createEventResponse.json \ "success").as[Boolean]

        // ack the notification
        Ok(s"""
             |<soapenv:Envelope xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xmlns:soapenv="http://schemas.xmlsoap.org/soap/envelope/">
             |    <soapenv:Body>
             |        <notificationsResponse xmlns="http://soap.sforce.com/2005/09/outbound">
             |            <Ack>$success</Ack>
             |        </notificationsResponse>
             |    </soapenv:Body>
             |</soapenv:Envelope>
           """.stripMargin).as("text/xml")
      }
    }
  }

  /*
   A Trigger in Salesforce calls this webhook which notifies IFTTT that an event happened for each of the users watching for events
    */
  def ifttt_event() = Action.async(parse.json) { request =>

    val orgId = (request.body \ "orgId").as[String]

    Global.redis.smembers[String](orgId).flatMap { watchers =>
        val json = Json.obj(
          "data" -> watchers.map { userId =>
            Json.obj("user_id" -> userId)
          }
        )

        WS.
          url("https://realtime.ifttt.com/v1/notifications").
          withHeaders("IFTTT-Channel-Key" -> Global.ifffChannelKey).
          post(json).
          map(r => Ok)
    }
  }

}