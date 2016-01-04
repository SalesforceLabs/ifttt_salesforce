package utils

import play.api.Play
import play.api.libs.json.JsObject
import play.api.test.{FakeApplication, PlaySpecification}

class ForceIFTTTSpec extends PlaySpecification with SingleInstance {

  implicit val app: FakeApplication = FakeApplication()

  lazy val authToken = {
    await {
      ForceUtils.login(ForceUtils.ENV_PROD, Play.current.configuration.getString("ifttt.test.username").get, Play.current.configuration.getString("ifttt.test.password").get).map { loginInfo =>
        (loginInfo \ "access_token").as[String]
      }
    }
  }

  lazy val userInfo = await(ForceUtils.userinfo(authToken))

  "opportunitiesWon" should {
    "get 1 opportunity" in {
      val json = await(ForceIFTTT.opportunitiesWon(authToken, 1))
      (json \ "data").as[Seq[JsObject]].length should beEqualTo (1)
      ((json \ "data").as[Seq[JsObject]].head \ "amount").asOpt[String] should beSome ("$1")
    }
  }
  
}