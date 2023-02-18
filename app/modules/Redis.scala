/*
 * Copyright (c) 2018, Salesforce.com, Inc.
 * All rights reserved.
 * SPDX-License-Identifier: BSD-3-Clause
 * For full license text, see the LICENSE.txt file in the repo root or https://opensource.org/licenses/BSD-3-Clause
 */

package modules

import java.net.URI
import akka.actor.ActorSystem
import com.redis.RedisClient
import org.apache.http.ssl.{SSLContexts, TrustStrategy}

import javax.inject.{Inject, Singleton}
import play.api.Configuration

import java.security.cert.X509Certificate

@Singleton
class Redis @Inject() (configuration: Configuration) (implicit actorSystem: ActorSystem) {

  val client = {
    val url = new URI(configuration.get[String]("redis.url"))

    val passwordOpt = Option(url.getUserInfo).flatMap(_.split(":").lastOption)

    val sslContext = SSLContexts
      .custom()
      .loadTrustMaterial(null, new TrustStrategy() {
        def isTrusted(arg0: Array[X509Certificate], arg1: String) = true
      })
      .build()

    new RedisClient(url.getHost, url.getPort, secret = passwordOpt, sslContext = Some(sslContext))
  }

  client.ping.get

}
