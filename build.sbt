lazy val root = (project in file(".")).enablePlugins(PlayScala, ForcePlugin)

name := "ifttt-salesforce"

scalaVersion := "2.12.7"

libraryDependencies ++= Seq(
  guice,
  ws,
  ehcache,
  "com.typesafe.play" %% "play-json-joda" % "2.7.0-RC2",
  "com.github.etaty" %% "rediscala" % "1.8.0",
  "com.github.t3hnar" %% "scala-bcrypt" % "3.1",
  "org.webjars" %% "webjars-play" % "2.7.0-RC3",
  "org.webjars" % "salesforce-lightning-design-system" % "2.5.2",
  specs2 % Test
)

username in Force := sys.env.getOrElse("SALESFORCE_USERNAME", "")

password in Force := sys.env.getOrElse("SALESFORCE_PASSWORD", "")

packagedComponents in Force := Seq("com.salesforce.ifttt")
