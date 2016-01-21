package utils

import play.api.Play
import play.api.libs.json.{JsObject, Json}
import play.api.test.{FakeApplication, PlaySpecification}
import utils.Force.ForceError

import scala.util.Try

class ForceSpec extends PlaySpecification with SingleInstance {

  implicit val app: FakeApplication = FakeApplication()

  lazy val authToken = {
    await {
      Force.login(Force.ENV_PROD, Play.current.configuration.getString("ifttt.test.username").get, Play.current.configuration.getString("ifttt.test.password").get).map { loginInfo =>
        (loginInfo \ "access_token").as[String]
      }
    }
  }

  lazy val userInfo = await(Force.userinfo(authToken))

  "chatterGroups" should {
    "fetch the list of chatter groups" in {
      val json = await(Force.chatterGroups(authToken, userInfo))
      json.value.size should beGreaterThan (0)
    }
  }

  "chatterPostMessage" should {
    "post to a user feed" in {
      val json = await(Force.chatterPostMessage(authToken, "test", None, None))
      (json._1 \ "id").asOpt[String] should beSome
    }
    "post to a user feed" in {
      val json = await(Force.chatterPostMessage(authToken, "test", None, Some("0F9j000000074BA")))
      (json._1 \ "id").asOpt[String] should beSome
    }
  }

  "chatterPostFile" should {
    "post a file to a user feed" in {
      val json = await(Force.chatterPostFile(authToken, "http://www.jamesward.com/images/james_ward_2015.jpg", "foooo.png", None, None, None))
      (json._1 \ "id").asOpt[String] should beSome
      (json._1 \ "capabilities" \ "content" \ "checksum").asOpt[String] should beSome("6a1fa22d29a720ac58930c3ababdf05b")
    }
  }

  "chatterPostLink" should {
    "post a link to a user feed" in {
      val json = await(Force.chatterPostLink(authToken, "http://www.jamesward.com", None, None, None))
      (json._1 \ "id").asOpt[String] should beSome
    }
  }

  "insertRecord" should {
    "work with html fields" in {
      val contact = Json.obj(
        "LastName" -> "Foo",
        "Rich_Text__c" -> "<b>test</b>"
      )
      val result = await(Force.insert(authToken, "Contact" , contact))
      (result \ "id").asOpt[String] should beSome
    }
    "work with a note" in {
      val note = Json.obj(
        "Title" -> "Foo",
        "ParentId" -> "003j000000iEshD",
        "Body" -> "<b>test</b>"
      )
      val result = await(Force.insert(authToken, "Note" , note))
      (result \ "id").asOpt[String] should beSome
    }
    "work with a ContentNote" in {
      val contentNote = Json.obj(
        "Title" -> "Foo",
        "Content" -> "<b>Foo</b>"
      )
      val result = await(Force.insert(authToken, "ContentNote" , contentNote))
      (result \ "id").asOpt[String] should beSome
    }
    "fail without the required fields" in {
      val contact = Json.obj("FirstName" -> "Foo")
      val error = ForceError(
        Json.obj(
          "errors" -> Json.arr(
            Json.obj(
              "message" -> "Required fields are missing: [LastName]"
            )
          )
        )
      )
      await(Force.insert(authToken, "Contact" , contact)) should throwA[ForceError](error)
    }
  }

  "query" should {
    "work with a valid query" in {
      val result = await(Force.query(authToken, userInfo, "SELECT Id FROM Contact"))
      (result \ "totalSize").as[Int] should beGreaterThan (0)
    }
    "not work with in invalid query" in {
      val error = ForceError(
        Json.obj(
          "errors" -> Json.arr(
            Json.obj(
              "status" -> "FOO"
            )
          )
        )
      )
      val result = Try(await(Force.query(authToken, userInfo, "FOO")))

      result should beAFailedTry.withThrowable[ForceError]

      (result.failed.get.asInstanceOf[ForceError].json \\ "errorCode").seq.map(_.as[String]) should beEqualTo (Seq("MALFORMED_QUERY"))
    }
  }

  "opportunitiesWon" should {
    "get 1 opportunity won" in {
      val result = await(Force.opportunitiesWon(authToken, userInfo, 1))
      (result \ "records").as[Seq[JsObject]].length should beEqualTo (1)
      ((result \ "records").as[Seq[JsObject]].head \ "Name").asOpt[String] should beSome ("Big Deal")
      ((result \ "records").as[Seq[JsObject]].head \ "Owner" \ "Name").asOpt[String] should beSome ("IFTTT Test")
    }
  }

  "communities" should {
    "get a list of communities" in {
      val result = await(Force.communities(authToken, userInfo))
      result.value.length should beGreaterThan (0)
    }
  }

  "communityGroups" should {
    "get a list of groups in a community" in {
      val result = await(Force.communityGroups(authToken, userInfo, "0DBj0000000L0F3GAK"))
      result.value.length should beGreaterThan (0)
    }
  }
  
}