package controllers.ifttt.v1

import org.junit.runner.RunWith
import org.specs2.matcher.JsonMatchers
import org.specs2.mutable._
import org.specs2.runner._
import play.api.libs.json.{JsObject, JsError, JsSuccess, Json}

import scala.util.parsing.json.{JSONObject, JSONArray}

@RunWith(classOf[JUnitRunner])
class TriggersSpec extends Specification with JsonMatchers {

  "Triggers" should {
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
          |      "LastModifiedDate":"2014-07-02T15:55:32.000+0000"
          |    }
          |  ]
          |}""".stripMargin)

      val actualIFTTTResponse = queryResult.transform(Triggers.queryResultToIFTTT).asEither

      actualIFTTTResponse must beRight { json: JsObject =>
        json.toString must /(
          "data" -> JSONArray(
            List(
              JSONObject(
                Map(
                  "amount" -> "$0",
                  "name" -> "asdf",
                  "meta" -> JSONObject(
                    Map(
                      "id" -> "006o0000002el8xAAA",
                      "timestamp" -> 1404280800
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

}
