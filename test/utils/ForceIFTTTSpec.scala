/*
 * Copyright (c) 2018, Salesforce.com, Inc.
 * All rights reserved.
 * SPDX-License-Identifier: BSD-3-Clause
 * For full license text, see the LICENSE.txt file in the repo root or https://opensource.org/licenses/BSD-3-Clause
 */

package utils

import play.api.libs.json.{JsObject, Json}
import play.api.test.{PlaySpecification, WithApplication}
import play.api.{Application, Configuration}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

class ForceIFTTTSpec extends PlaySpecification {

  def force(implicit app: Application): Force = {
    app.injector.instanceOf[Force]
  }

  def forceIFTTT(implicit app: Application): ForceIFTTT = {
    app.injector.instanceOf[ForceIFTTT]
  }

  def authToken(implicit app: Application) = {
    val configuration = app.injector.instanceOf[Configuration]
    await {
      force.login(Force.ENV_PROD, configuration.get[String]("ifttt.test.username"), configuration.get[String]("ifttt.test.password")).map { loginInfo =>
        (loginInfo \ "access_token").as[String]
      }
    }
  }

  def userInfo(implicit app: Application) = await(force.userinfo(authToken))

  "opportunitiesWon" should {
    "get 1 opportunity" in new WithApplication() {
      val json = await(forceIFTTT.opportunitiesWon(authToken, 1))
      (json \ "data").as[Seq[JsObject]].length should beEqualTo (1)
      ((json \ "data").as[Seq[JsObject]].head \ "amount").asOpt[String] should beSome ("$1")
    }
  }

  "allGroups" should {
    "get all the groups" in new WithApplication() {
      val json = await(forceIFTTT.allGroups(authToken))
      (json \ "data").as[Seq[JsObject]].length should beGreaterThan (1)
      ((json \ "data").as[Seq[JsObject]].head \ "label").asOpt[String] should beSome ("Foo")
      ((json \ "data").as[Seq[JsObject]].head \ "value").asOpt[String] should beSome ("0F9j000000074BACAY")
    }
  }

  "query" should {
    "work" in new WithApplication() {
      val query = s"""
                |SELECT Id, LastModifiedDate
                |FROM Contact
                |ORDER BY LastModifiedDate DESC
      """.stripMargin

      val json = await(forceIFTTT.query(authToken, query))

      (json \ "data").as[Seq[JsObject]].length should beGreaterThan (0)
    }
    "produce new ids when a record is updated" in new WithApplication() {
      val query = s"""
                     |SELECT Id, LastModifiedDate
                     |FROM Contact
                     |ORDER BY LastModifiedDate DESC
      """.stripMargin

      val initialContact = (await(forceIFTTT.query(authToken, query)) \ "data").as[Seq[JsObject]].head

      val id = (initialContact \ "id").as[String]
      val initialIftttId = (initialContact \ "meta" \ "id").as[String]

      val randomLastName = Random.alphanumeric.take(8).mkString
      val contactJson = Json.obj("LastName" -> randomLastName)

      await(force.update(authToken, "Contact", id, contactJson))

      val updatedContact = (await(forceIFTTT.query(authToken, query)) \ "data").as[Seq[JsObject]].find { json =>
        (json \ "id").as[String] == id
      }.get

      val updatedIftttId = (updatedContact \ "meta" \ "id").as[String]

      initialIftttId shouldNotEqual updatedIftttId
    }
  }

}
