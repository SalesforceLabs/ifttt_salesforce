/*
 * Copyright (c) 2018, Salesforce.com, Inc.
 * All rights reserved.
 * SPDX-License-Identifier: BSD-3-Clause
 * For full license text, see the LICENSE.txt file in the repo root or https://opensource.org/licenses/BSD-3-Clause
 */

package controllers

import javax.inject.Inject
import modules.Redis
import play.api.mvc.InjectedController
import utils.Force

import scala.concurrent.{ExecutionContext, Future}

class Application @Inject()
  (force: Force, redis: Redis)
  (indexView: views.html.index, installView: views.html.install, helpView: views.html.help, authorizeErrorsView: views.html.authorizeErrors, packageNotFoundView: views.html.packageNotFound, errorsView: views.html.errors)
  (implicit ec: ExecutionContext) extends InjectedController {

  def index() = Action {
    Ok(indexView())
  }

  def install = Action {
    Ok(installView())
  }

  def help = Action {
    Ok(helpView())
  }

  def errors = Action.async { implicit request =>
    request.flash.get("access_token").fold {
      val redirUrl = routes.OAuth2.authorized().absoluteURL()
      val qs = s"client_id=${force.salesforceOauthKey}&state=local-errors&response_type=code&prompt=login&redirect_uri=$redirUrl"

      Future.successful(Ok(authorizeErrorsView(qs)))
    } { accessToken =>
      val auth = s"Bearer $accessToken"
      force.userinfo(auth).map { userInfo =>
        val userId = (userInfo \ "user_id").as[String]

        val errors = redis.client.lrange[String](userId, 0, -1).getOrElse(List.empty)
        val friendlyErrors = errors.distinct.map {
          case error if error.contains("Object type 'ifttt__IFTTT_Event__c' is not supported") =>
            packageNotFoundView().toString()
          case error =>
            val msg = error.getOrElse("Error")
            views.html.standardError(msg).toString()
        }

        Ok(errorsView(friendlyErrors)).flash("access_token" -> accessToken)
      } recoverWith force.standardErrorHandler(auth)
    }
  }

  def errorsClear = Action.async(parse.formUrlEncoded) { request =>
    request.flash.get("access_token").fold {
      Future.successful(Unauthorized("No access token in scope"))
    } { accessToken =>
      val auth = s"Bearer $accessToken"
      force.userinfo(auth).map { userInfo =>
        val userId = (userInfo \ "user_id").as[String]

        redis.client.del(userId)
        Redirect(routes.Application.errors()).flash("access_token" -> accessToken)
      } recoverWith force.standardErrorHandler(auth)
    }
  }

}
