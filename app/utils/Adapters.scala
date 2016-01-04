package utils

import play.api.libs.json._
import play.api.libs.json.Reads._
import play.api.libs.functional.syntax._

object Adapters {

  def anyJsValueToSalesforce(jsValue: JsValue): JsValue = {

    // July 10, 2015 at 8:00AM
    val googleDateTimeReads: Reads[JsValue] = jodaDateReads("MMMM dd, YYYY 'at' hh:mma").map { dateTime =>
      JsNumber(dateTime.getMillis)
    }

    val booleanStringReads: Reads[JsValue] = StringReads.filter { s =>
      s.equalsIgnoreCase("true") || s.equalsIgnoreCase("false")
    } map { s =>
      JsBoolean(s.toBoolean)
    }

    val allReads: Reads[JsValue] = googleDateTimeReads or booleanStringReads

    val maybeAdapted: JsResult[JsValue] = jsValue.transform(allReads)

    maybeAdapted.getOrElse(jsValue)
  }

}
