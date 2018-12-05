package utils

import play.api.libs.json.{JsObject, Json}
import play.api.test.{PlaySpecification, WithApplication}
import play.api.{Application, Configuration}
import utils.Force.{ForceError, UnauthorizedException}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

class ForceSpec extends PlaySpecification {

  def force(implicit app: Application): Force = {
    app.injector.instanceOf[Force]
  }

  def authToken(implicit app: Application) = {
    val configuration = app.injector.instanceOf[Configuration]
    await {
      force.login(Force.ENV_PROD, configuration.get[String]("ifttt.test.username"), configuration.get[String]("ifttt.test.password")).map { loginInfo =>
        (loginInfo \ "access_token").as[String]
      }
    }
  }

  def userInfo(implicit app: Application) = await(force.userinfo(authToken))

  "login" should {
    "fail with invalid credentials" in new WithApplication() {
      await(force.login(Force.ENV_PROD, "foo", "bar")) should throwA[UnauthorizedException]("authentication failure")
    }
  }

  "chatterGroups" should {
    "fetch the list of chatter groups" in new WithApplication() {
      val json = await(force.chatterGroups(authToken, userInfo))
      json.value.size should beGreaterThan (0)
    }
  }

  "chatterPostMessage" should {
    "post to a user feed" in new WithApplication() {
      val json = await(force.chatterPostMessage(authToken, "test", None, None))
      (json._1 \ "id").asOpt[String] should beSome
    }
    "post to a user feed" in new WithApplication() {
      val json = await(force.chatterPostMessage(authToken, "test", None, Some("0F9j000000074BA")))
      (json._1 \ "id").asOpt[String] should beSome
    }
  }

  "chatterPostFile" should {
    "post a file to a user feed" in new WithApplication() {
      val json = await(force.chatterPostFile(authToken, "http://www.jamesward.com/images/james_ward_2015.jpg", "foooo.png", None, None, None))
      (json._1 \ "id").asOpt[String] should beSome
      (json._1 \ "capabilities" \ "content" \ "checksum").asOpt[String] should beSome("6a1fa22d29a720ac58930c3ababdf05b")
    }
  }

  "chatterPostLink" should {
    "post a link to a user feed" in new WithApplication() {
      val json = await(force.chatterPostLink(authToken, "http://www.jamesward.com", None, None, None))
      (json._1 \ "id").asOpt[String] should beSome
    }
  }

  "insertRecord" should {
    "work with html fields" in new WithApplication() {
      val contact = Json.obj(
        "LastName" -> "Foo",
        "Rich_Text__c" -> "<b>test</b>"
      )
      val result = await(force.insert(authToken, "Contact" , contact))
      (result \ "id").asOpt[String] should beSome
    }
    "work with a note" in new WithApplication() {
      val note = Json.obj(
        "Title" -> "Foo",
        "ParentId" -> "003j000000iEshD",
        "Body" -> "<b>test</b>"
      )
      val result = await(force.insert(authToken, "Note" , note))
      (result \ "id").asOpt[String] should beSome
    }
    "work with a ContentNote" in new WithApplication() {
      val contentNote = Json.obj(
        "Title" -> "Foo",
        "Content" -> "<b>Foo</b>"
      )
      val result = await(force.insert(authToken, "ContentNote" , contentNote))
      (result \ "id").asOpt[String] should beSome
    }
    "fail without the required fields" in new WithApplication() {
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
      await(force.insert(authToken, "Contact" , contact)) should throwA[ForceError](error)
    }
  }

  "update" should {
    "work" in new WithApplication() {
      val contact = Json.obj("LastName" -> "Foo")
      val insert = await(force.insert(authToken, "Contact", contact))
      val id = (insert \ "id").as[String]

      val contactUpdate = Json.obj("LastName" -> "Bar")
      await(force.update(authToken, "Contact", id, contactUpdate)) should beEqualTo (())
    }
  }

  "query" should {
    "not work with an invalid auth token" in new WithApplication() {
      await(force.query("asdf", userInfo, "SELECT Id FROM Contact")) should throwA[UnauthorizedException]("Session expired or invalid")
    }
    "work with a valid query" in new WithApplication() {
      val result = await(force.query(authToken, userInfo, "SELECT Id FROM Contact"))
      (result \ "totalSize").as[Int] should beGreaterThan (0)
    }
    "not work with in invalid query" in new WithApplication() {
      val error = ForceError(
        Json.obj(
          "errors" -> Json.arr(
            Json.obj(
              "status" -> "FOO"
            )
          )
        )
      )
      val result = Try(await(force.query(authToken, userInfo, "FOO")))

      result should beAFailedTry.withThrowable[ForceError]

      (result.failed.get.asInstanceOf[ForceError].json \\ "errorCode").seq.map(_.as[String]) should beEqualTo (Seq("MALFORMED_QUERY"))
    }
  }

  "opportunitiesWon" should {
    "get 1 opportunity won" in new WithApplication() {
      val result = await(force.opportunitiesWon(authToken, userInfo, 1))
      (result \ "records").as[Seq[JsObject]].length should beEqualTo (1)
      ((result \ "records").as[Seq[JsObject]].head \ "Name").asOpt[String] should beSome ("Big Deal")
      ((result \ "records").as[Seq[JsObject]].head \ "Owner" \ "Name").asOpt[String] should beSome ("IFTTT Test")
    }
  }

  "communities" should {
    "get a list of communities" in new WithApplication() {
      val result = await(force.communities(authToken, userInfo))
      result.value.length should beGreaterThan (0)
    }
  }

  "communityGroups" should {
    "get a list of groups in a community" in new WithApplication() {
      val result = await(force.communityGroups(authToken, userInfo, "0DBj0000000L0F3GAK"))
      result.value.length should beGreaterThan (0)
    }
  }

  "describe" should {
    "describe an SObject" in new WithApplication() {
      val result = await(force.describe(authToken, "Contact"))
      (result \ "label").as[String] must beEqualTo ("Contact")
    }
  }

}
