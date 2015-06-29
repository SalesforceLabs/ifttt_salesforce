package controllers

import play.api.Play
import play.api.mvc.{Action, Controller}

object Application extends Controller {

  lazy val managedPackageId = Play.current.configuration.getString("salesforce.managed-package-id").get

  def index() = Action {
    Ok(views.html.index(managedPackageId))
  }

}
