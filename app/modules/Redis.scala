package modules

import java.net.URI

import akka.actor.ActorSystem
import javax.inject.{Inject, Singleton}
import play.api.Configuration
import redis.RedisClient

import scala.concurrent.Await
import scala.concurrent.duration._

@Singleton
class Redis @Inject() (configuration: Configuration) (implicit actorSystem: ActorSystem) {

  val client = {
    val url = new URI(configuration.get[String]("redis.url"))

    val passwordOpt = Option(url.getUserInfo).flatMap(_.split(":").lastOption)

    RedisClient(url.getHost, url.getPort, passwordOpt)
  }

  Await.result(client.ping(), 30.seconds)

}
