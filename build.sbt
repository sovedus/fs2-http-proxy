lazy val tlScalafixVersion = "0.5.0"
lazy val log4catsVersion = "2.7.1"

ThisBuild / tlBaseVersion := "0.1"
ThisBuild / organization := "io.github.sovedus"
ThisBuild / organizationName := "Sovedus"
ThisBuild / startYear := Some(2025)
ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers := List(
  tlGitHubDev("sovedus", "")
)
ThisBuild / tlCiReleaseBranches := Seq("master")
ThisBuild / githubWorkflowTargetBranches := Seq("master")
ThisBuild / githubWorkflowJavaVersions := Seq(JavaSpec.temurin("11"))

ThisBuild / scalafixDependencies ++= Seq(
  "org.typelevel" %% "typelevel-scalafix" % tlScalafixVersion,
  "org.typelevel" %% "typelevel-scalafix-cats" % tlScalafixVersion,
  "org.typelevel" %% "typelevel-scalafix-cats-effect" % tlScalafixVersion,
  "org.typelevel" %% "typelevel-scalafix-fs2" % tlScalafixVersion,
  "org.typelevel" %% "typelevel-scalafix-http4s" % tlScalafixVersion
)
ThisBuild / semanticdbOptions ++= Seq("-P:semanticdb:synthetics:on")

ThisBuild / scalaVersion := "2.13.16"

addCommandAlias("fmt", "scalafmtAll;scalafmtSbt")

lazy val root = (project in file(".")).settings(
  name := "fs2-http-proxy",
  Test / fork := true,
  semanticdbEnabled := true,
  semanticdbVersion := scalafixSemanticdb.revision,
  libraryDependencies ++= Seq(
    "co.fs2" %% "fs2-io" % "3.12.0",
    "org.typelevel" %% "cats-parse" % "1.1.0",
    "org.typelevel" %% "case-insensitive" % "1.5.0",
    "org.typelevel" %% "log4cats-core" % log4catsVersion,
    "org.http4s" %% "http4s-core" % "0.23.30",
    "org.typelevel" %% "log4cats-slf4j" % log4catsVersion % Test,
    "ch.qos.logback" % "logback-classic" % "1.5.18" % Test,
    "org.scalatest" %% "scalatest" % "3.2.19" % Test,
    "org.typelevel" %% "cats-effect-testing-scalatest" % "1.6.0" % Test,
    "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % Test
  )
)
