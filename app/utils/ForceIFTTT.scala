package utils

import play.api.data.validation.ValidationError
import play.api.libs.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object ForceIFTTT {

  def opportunitiesWon(auth: String, limit: Int): Future[JsObject] = {
    for {
      userInfo <- Force.userinfo(auth)
      salesforceJson <- Force.opportunitiesWon(auth, userInfo, limit)
      iftttJson <- salesforceJson.transform(Adapters.opportunityWonQueryResultToIFTTT(Force.instanceUrl(userInfo))).toFuture
    } yield iftttJson
  }

  def query(auth: String, query: String): Future[JsObject] = {
    for {
      userInfo <- Force.userinfo(auth)
      salesforceJson <- Force.query(auth, userInfo, query)
      iftttJson <- salesforceJson.transform(Adapters.anyQueryResultToIFTTT(Force.instanceUrl(userInfo))).toFuture
    } yield iftttJson
  }

  def iftttEventQuery(auth: String, query: String): Future[JsObject] = {
    for {
      userInfo <- Force.userinfo(auth)
      salesforceJson <- Force.query(auth, userInfo, query)
      iftttJson <- salesforceJson.transform(Adapters.iftttEventQueryResultToIFTTT).toFuture
    } yield iftttJson
  }

  def allGroups(auth: String): Future[JsObject] = {

    def sortByLabel(jsArray: JsArray): Seq[JsValue] = jsArray.value.sortBy(_.\("label").as[String])

    def communitiesGroupsFuture(userInfo: JsValue, communities: JsArray): Future[Seq[JsValue]] = {
      Future.sequence {
        communities.value.map { community =>
          val communityId = (community \ "id").as[String]
          val communityName = (community \ "name").as[String]

          Force.communityGroups(auth, userInfo, communityId).flatMap { groups =>
            groups.transform(Adapters.salesforceGroupsToIFTTT).map(sortByLabel).fold(
              errors => Future.failed(JsTransformError(errors)),
              groupsIfttt => Future.successful(Json.obj("label" -> s"Community: $communityName", "values" -> groupsIfttt))
            )
          }
        }
      }
    }

    for {
      userInfo <- Force.userinfo(auth)
      orgGroups <- Force.chatterGroups(auth, userInfo)
      orgGroupsIfttt <- orgGroups.transform(Adapters.salesforceGroupsToIFTTT).map(sortByLabel).toFuture
      communities <- Force.communities(auth, userInfo)
      communitiesGroupsIfttt <- communitiesGroupsFuture(userInfo, communities)
    } yield Json.obj("data" -> (orgGroupsIfttt ++ communitiesGroupsIfttt))
  }

  implicit class JsResultToFuture[A](val value: JsResult[A]) extends AnyVal {
    def toFuture = value.fold(errors => Future.failed(JsTransformError(errors)), Future.successful)
  }

  case class JsTransformError(errors: Seq[(JsPath, Seq[ValidationError])]) extends Exception {
    override def getMessage = {
      // todo
      "Json transformation error"
    }
  }

}
