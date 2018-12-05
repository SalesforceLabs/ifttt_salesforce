package utils

import javax.inject.Inject
import play.api.Configuration
import play.api.libs.json._

import scala.concurrent.{ExecutionContext, Future}

class ForceIFTTT @Inject() (configuration: Configuration, force: Force) (implicit ec: ExecutionContext) {

  import ForceIFTTT._

  lazy val ifffChannelKey = configuration.get[String]("ifttt.channel.key")
  lazy val ifffChannelId = configuration.get[String]("ifttt.channel.id")


  def opportunitiesWon(auth: String, limit: Int): Future[JsObject] = {
    for {
      userInfo <- force.userinfo(auth)
      salesforceJson <- force.opportunitiesWon(auth, userInfo, limit)
      iftttJson <- salesforceJson.transform(Adapters.opportunityWonQueryResultToIFTTT(force.instanceUrl(userInfo))).toFuture
    } yield iftttJson
  }

  def query(auth: String, query: String): Future[JsObject] = {
    for {
      userInfo <- force.userinfo(auth)
      salesforceJson <- force.query(auth, userInfo, query)
      iftttJson <- salesforceJson.transform(Adapters.anyQueryResultToIFTTT(force.instanceUrl(userInfo))).toFuture
    } yield iftttJson
  }

  def iftttEventQuery(auth: String, query: String): Future[JsObject] = {
    for {
      userInfo <- force.userinfo(auth)
      salesforceJson <- force.query(auth, userInfo, query)
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

          force.communityGroups(auth, userInfo, communityId).flatMap { groups =>
            groups.transform(Adapters.salesforceGroupsToIFTTT).map(sortByLabel).fold(
              errors => Future.failed(JsTransformError(errors)),
              groupsIfttt => Future.successful(Json.obj("label" -> s"Community: $communityName", "values" -> groupsIfttt))
            )
          }
        }
      }
    }

    for {
      userInfo <- force.userinfo(auth)
      orgGroups <- force.chatterGroups(auth, userInfo)
      orgGroupsIfttt <- orgGroups.transform(Adapters.salesforceGroupsToIFTTT).map(sortByLabel).toFuture
      communities <- force.communities(auth, userInfo)
      communitiesGroupsIfttt <- communitiesGroupsFuture(userInfo, communities)
    } yield Json.obj("data" -> (orgGroupsIfttt ++ communitiesGroupsIfttt))
  }


}

object ForceIFTTT {

  implicit class JsResultToFuture[A](val value: JsResult[A]) extends AnyVal {
    def toFuture = value.fold(errors => Future.failed(JsTransformError(errors)), Future.successful)
  }

  case class JsTransformError(errors: Seq[(JsPath, Seq[JsonValidationError])]) extends Exception {
    override def getMessage = {
      // todo
      "Json transformation error"
    }
  }

}
