package utils

import org.junit.runner.RunWith
import org.specs2.matcher.JsonMatchers
import org.specs2.mutable._
import org.specs2.runner._
import play.api.libs.json.JsString

@RunWith(classOf[JUnitRunner])
class AdaptersSpec extends Specification with JsonMatchers {

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
