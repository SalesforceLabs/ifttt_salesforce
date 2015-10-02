package utils

import org.specs2.specification.BeforeExample
import play.api.Play
import play.api.test.{FakeApplication, PlaySpecification}

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
  
}

trait SingleInstance extends BeforeExample {
  def before {
    if (Play.unsafeApplication == null) Play.start(App)
  }
}

object App extends FakeApplication()