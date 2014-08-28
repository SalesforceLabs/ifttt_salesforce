package controllers

import play.api.Play
import play.api.mvc.Controller

object StaticWebJarAssets extends Controller {

  def getUrl(path: String): String = {
    val prefix = Play.current.configuration.getString("content.url").getOrElse("")
    prefix + routes.WebJarAssets.at(WebJarAssets.locate(path))
  }

}
