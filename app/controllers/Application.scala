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
import utils.{Crypto, Force}

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

  def errors = Action.async { request =>
    request.flash.get("enc_access_token").fold {
      val redirUrl = routes.OAuth2.authorized().absoluteURL(secure = request.secure)(request)
      val qs = s"client_id=${force.salesforceOauthKey}&state=local-errors&response_type=code&prompt=login&redirect_uri=$redirUrl"

      Future.successful(Ok(authorizeErrorsView(qs)))
    } { encAccessToken =>
      val accessToken = Crypto.decryptAES(encAccessToken)
      val auth = s"Bearer $accessToken"
      force.userinfo(auth).flatMap { userInfo =>
        val userId = (userInfo \ "user_id").as[String]

        redis.client.lrange[String](userId, 0, -1).map { errors =>
          val friendlyErrors = errors.distinct.map {
            case error if error.contains("Object type 'ifttt__IFTTT_Event__c' is not supported") =>
              packageNotFoundView().toString()
            case error =>
              views.html.standardError(error).toString()
          }

          Ok(errorsView(friendlyErrors, encAccessToken))
        }
      } recoverWith force.standardErrorHandler(auth)
    }
  }

  def errorsClear = Action.async(parse.formUrlEncoded) { request =>
    request.body.get("enc_access_token").flatMap(_.headOption).fold {
      Future.successful(Unauthorized("No access token in scope"))
    } { encAccessToken =>
      val accessToken = Crypto.decryptAES(encAccessToken)
      val auth = s"Bearer $accessToken"
      force.userinfo(auth).flatMap { userInfo =>
        val userId = (userInfo \ "user_id").as[String]

        redis.client.del(userId).map { errors =>
          Redirect(routes.Application.errors()).flash("enc_access_token" -> encAccessToken)
        }
      } recoverWith force.standardErrorHandler(auth)
    }
  }

}
