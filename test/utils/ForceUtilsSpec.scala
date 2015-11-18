package utils

import org.specs2.specification.BeforeExample
import play.api.Play
import play.api.libs.json.Json
import play.api.test.{FakeApplication, PlaySpecification}
import utils.ForceUtils.ForceError

class ForceUtilsSpec extends PlaySpecification with SingleInstance {

  implicit val app: FakeApplication = FakeApplication()

  lazy val authToken = {
    await {
      ForceUtils.login(ForceUtils.ENV_PROD, Play.current.configuration.getString("ifttt.test.username").get, Play.current.configuration.getString("ifttt.test.password").get).map { loginInfo =>
        (loginInfo \ "access_token").as[String]
      }
    }
  }
  
  "chatterGroups" should {
    "fetch the list of chatter groups" in {
      val json = await(ForceUtils.chatterGroups(authToken))
      json.value.size should beGreaterThan (0)
    }
  }

  "chatterPostMessage" should {
    "post to a user feed" in {
      val json = await(ForceUtils.chatterPostMessage(authToken, "test", None))
      (json._1 \ "id").asOpt[String] should beSome
    }
    "post to a user feed" in {
      val json = await(ForceUtils.chatterPostMessage(authToken, "test", Some("0F9j000000074BA")))
      (json._1 \ "id").asOpt[String] should beSome
    }
  }

  "chatterPostFile" should {
    "post a file to a user feed" in {
      val json = await(ForceUtils.chatterPostFile(authToken, "http://www.jamesward.com/images/james_ward_2015.jpg", "foooo.png", None, None))
      (json._1 \ "id").asOpt[String] should beSome
      (json._1 \ "capabilities" \ "content" \ "checksum").asOpt[String] should beSome("6a1fa22d29a720ac58930c3ababdf05b")
    }
  }

  "chatterPostLink" should {
    "post a link to a user feed" in {
      val json = await(ForceUtils.chatterPostLink(authToken, "http://www.jamesward.com", None, None))
      (json._1 \ "id").asOpt[String] should beSome
    }
  }

  "insertRecord" should {
    "work with html fields" in {
      val contact = Json.obj(
        "LastName" -> "Foo",
        "Rich_Text__c" -> "<b>test</b>"
      )
      val result = await(ForceUtils.insert(authToken, "Contact" , contact))
      (result \ "id").asOpt[String] should beSome
    }
    "work with a note" in {
      val note = Json.obj(
        "Title" -> "Foo",
        "ParentId" -> "003j000000iEshD",
        "Body" -> "<b>test</b>"
      )
      val result = await(ForceUtils.insert(authToken, "Note" , note))
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
      await(ForceUtils.insert(authToken, "Contact" , contact)) should throwA[ForceError](error)
    }
  }
  
}

trait SingleInstance extends BeforeExample {
  def before {
    if (Play.unsafeApplication == null) Play.start(App)
  }
}

object App extends FakeApplication()