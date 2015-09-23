package utils

import org.specs2.specification.BeforeExample
import play.api.Play
import play.api.test.{FakeApplication, PlaySpecification}
import play.api.test.Helpers._

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

}

trait SingleInstance extends BeforeExample {
  def before {
    if (Play.unsafeApplication == null) Play.start(App)
  }
}

object App extends FakeApplication()