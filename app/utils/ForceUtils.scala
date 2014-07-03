package utils

import play.api.http.HeaderNames
import play.api.libs.ws.WS
import play.api.Play.current
import scala.concurrent.ExecutionContext.Implicits.global

object ForceUtils {

  def userinfo(auth: String) = {
    WS.
      url("https://login.salesforce.com/services/oauth2/userinfo").
      withHeaders(HeaderNames.AUTHORIZATION -> auth).
      get()
  }


}
