name := """ifttt-salesforce"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalaVersion := "2.11.2"

resolvers += "rediscala" at "http://dl.bintray.com/etaty/maven"

libraryDependencies ++= Seq(
  ws,
  "com.etaty.rediscala" %% "rediscala" % "1.3.1"
)
