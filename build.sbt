import de.heikoseeberger.sbtheader.FileType

lazy val root = (project in file(".")).enablePlugins(PlayScala, ForcePlugin, AutomateHeaderPlugin)

name := "ifttt-salesforce"

scalaVersion := "2.12.8"

scalacOptions ++= Seq("-deprecation", "-feature")

libraryDependencies ++= Seq(
  guice,
  ws,
  ehcache,
  "com.typesafe.play" %% "play-json-joda" % "2.7.0-RC2",
  "com.github.etaty" %% "rediscala" % "1.9.0",
  "com.github.t3hnar" %% "scala-bcrypt" % "3.1",
  "org.webjars" %% "webjars-play" % "2.7.0-RC3",
  "org.webjars" % "salesforce-lightning-design-system" % "2.5.2",
  specs2 % Test
)

pipelineStages := Seq(digest, gzip)

username in Force := sys.env.getOrElse("SALESFORCE_USERNAME", "")

password in Force := sys.env.getOrElse("SALESFORCE_PASSWORD", "")

packagedComponents in Force := Seq("com.salesforce.ifttt")

// license header stuff

organizationName := "Salesforce.com, Inc."

startYear := Some(2018)

licenses += "BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause")

headerMappings += FileType("html") -> HeaderCommentStyle.twirlStyleBlockComment

headerLicense := Some(
  HeaderLicense.Custom(
    """|Copyright (c) 2018, Salesforce.com, Inc.
       |All rights reserved.
       |SPDX-License-Identifier: BSD-3-Clause
       |For full license text, see the LICENSE.txt file in the repo root or https://opensource.org/licenses/BSD-3-Clause
       |""".stripMargin
  )
)

headerSources.in(Compile) ++= sources.in(Compile, TwirlKeys.compileTemplates).value
