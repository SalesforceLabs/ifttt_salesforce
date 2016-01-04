package utils

import org.specs2.matcher.JsonMatchers
import org.specs2.mutable._
import play.api.libs.json.{JsObject, JsString, Json}

import scala.util.parsing.json.{JSONArray, JSONObject}

class AdaptersSpec extends Specification with JsonMatchers {

  "opportunityWonQueryResultToIFTTT" should {
    "transform salesforce records into ifttt trigger data" in {

      val queryResult = Json.parse(
        """{"totalSize":1,
          |  "done":true,
          |  "records":[
          |    {
          |      "attributes": {"type":"Opportunity","url":"/services/data/v30.0/sobjects/Opportunity/006o0000002el8xAAA"},
          |      "Id":"006o0000002el8xAAA",
          |      "Name":"asdf",
          |      "Amount":null,
          |      "LastModifiedDate":"2014-07-02T15:55:32.000+0000",
          |      "Owner": {
          |        "Name": "Jon Doe"
          |      }
          |    }
          |  ]
          |}""".stripMargin)

      val actualIFTTTResponse = queryResult.transform(Adapters.opportunityWonQueryResultToIFTTT("")).asEither

      actualIFTTTResponse must beRight { json: JsObject =>

        json.toString must /(
          "data" -> JSONArray(
            List(
              JSONObject(
                Map(
                  "amount" -> "$0",
                  "name" -> "asdf",
                  "link_to_opportunity" -> "006o0000002el8xAAA",
                  "timestamp" -> "2014-07-02T09:55:32.000-06:00",
                  "owner_name" -> "Jon Doe",
                  "meta" -> JSONObject(
                    Map(
                      "id" -> "006o0000002el8xAAA",
                      "timestamp" -> 1404316532D
                    )
                  )
                )
              )
            )
          )
        )
      }

    }
  }

  "anyJsValueToSalesforce" should {
    "transform a google date" in {
      Adapters.anyJsValueToSalesforce(JsString("July 10, 2015 at 8:00AM")).asOpt[Long] must beSome(1436536800000L)
      Adapters.anyJsValueToSalesforce(JsString("July 10, 2015 at 10:00PM")).asOpt[Long] must beSome(1436587200000L)
    }
    "not transform text" in {
      Adapters.anyJsValueToSalesforce(JsString("hello, world")).asOpt[String] must beSome("hello, world")
    }
    "transform booleans" in {
      Adapters.anyJsValueToSalesforce(JsString("true")).asOpt[Boolean] must beSome(true)
      Adapters.anyJsValueToSalesforce(JsString("false")).asOpt[Boolean] must beSome(false)
    }
  }

}
