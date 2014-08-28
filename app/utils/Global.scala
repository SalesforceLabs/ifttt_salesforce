package utils

import java.net.URI

import play.api.libs.concurrent.Akka
import play.api.{Play, Application, GlobalSettings}
import redis.RedisClient

import scala.util.Try

object Global extends GlobalSettings {

  lazy val redis = {
    val redisUrl = new URI(Play.current.configuration.getString("redis.url").get)

    val passwordOpt = Option(redisUrl.getUserInfo).flatMap(_.split(":").lastOption)

    RedisClient(redisUrl.getHost, redisUrl.getPort, passwordOpt)(Akka.system(Play.current))
  }

  lazy val ifffChannelKey = Play.current.configuration.getString("ifttt.channel.key").get

  lazy val ifffChannelId = Play.current.configuration.getString("ifttt.channel.id").get

  override def onStart(app: Application): Unit = {
    redis.ping()
    super.onStart(app)
  }

}
