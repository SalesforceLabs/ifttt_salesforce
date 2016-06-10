name := "ifttt-salesforce"

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala, ForcePlugin)

scalaVersion := "2.11.7"

resolvers += "rediscala" at "http://dl.bintray.com/etaty/maven"

libraryDependencies ++= Seq(
  ws,
  cache,
  "com.etaty.rediscala" %% "rediscala" % "1.4.0",
  "com.github.t3hnar" %% "scala-bcrypt" % "2.6",
  "org.webjars" %% "webjars-play" % "2.3.0-3",
  "org.webjars" % "salesforce-lightning-design-system" % "0.9.2"
)

username in Force := sys.env.getOrElse("SALESFORCE_USERNAME", "")

password in Force := sys.env.getOrElse("SALESFORCE_PASSWORD", "")

packagedComponents in Force := Seq("com.salesforce.ifttt")
