package controllers.ifttt.v1

import play.api.Play
import play.api.mvc.{Controller, Action}

object Application extends Controller {

  def status = Action { request =>
    val maybeOk = for {
      received <- request.headers.get("IFTTT-Channel-Key")
      expected <- Play.current.configuration.getString("ifttt.channel.key")
      if received == expected
    } yield {
      Ok
    }

    maybeOk.getOrElse(Unauthorized)
  }

}