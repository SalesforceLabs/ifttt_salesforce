package utils

import org.joda.time.DateTime
import play.api.libs.json.{JsNumber, JsValue}
import play.api.libs.json.Reads._

object Adapters {

  def anyJsValueToSalesforce(jsValue: JsValue): JsValue = {

    // July 10, 2015 at 8:00AM
    val googleDateTime = jodaDateReads("MMMM dd, YYYY 'at' hh:mma")

    val maybeAdapted = jsValue.asOpt[DateTime](googleDateTime).map(dateTime => JsNumber(dateTime.getMillis))

    maybeAdapted.getOrElse(jsValue)
  }

}
