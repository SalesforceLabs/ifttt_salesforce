/*
 * Copyright (c) 2018, Salesforce.com, Inc.
 * All rights reserved.
 * SPDX-License-Identifier: BSD-3-Clause
 * For full license text, see the LICENSE.txt file in the repo root or https://opensource.org/licenses/BSD-3-Clause
 */

package controllers.ifttt.v1

import javax.inject.Inject
import play.api.Configuration
import play.api.mvc.InjectedController

class Application @Inject() (configuration: Configuration) extends InjectedController {

  def status = Action { request =>
    val maybeOk = for {
      received <- request.headers.get("IFTTT-Channel-Key")
      expected <- configuration.getOptional[String]("ifttt.channel.key")
      if received == expected
    } yield {
      Ok
    }

    maybeOk.getOrElse(Unauthorized)
  }

}
