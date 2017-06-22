lazy val root = (project in file(".")).enablePlugins(PlayScala, ForcePlugin)

name := "ifttt-salesforce"

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.11"

libraryDependencies ++= Seq(
  ws,
  cache,
  "com.github.etaty" %% "rediscala" % "1.7.0",
  "com.github.t3hnar" %% "scala-bcrypt" % "2.6",
  "org.webjars" %% "webjars-play" % "2.3.0-3",
  "org.webjars" % "salesforce-lightning-design-system" % "0.9.2"
)

username in Force := sys.env.getOrElse("SALESFORCE_USERNAME", "")

password in Force := sys.env.getOrElse("SALESFORCE_PASSWORD", "")

packagedComponents in Force := Seq("com.salesforce.ifttt")
