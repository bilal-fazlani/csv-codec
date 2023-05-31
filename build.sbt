val scala3Version = "3.2.2"

ThisBuild / organization := "com.bilal-fazlani"
ThisBuild / organizationName := "Bilal Fazlani"

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/bilal-fazlani/csv-codec"),
    "https://github.com/bilal-fazlani/csv-codec.git"
  )
)
ThisBuild / developers := List(
  Developer(
    "bilal-fazlani",
    "Bilal Fazlani",
    "bilal.m.fazlani@gmail.com",
    url("https://bilal-fazlani.com")
  )
)
ThisBuild / licenses := List(
  "MIT License" -> url(
    "https://github.com/bilal-fazlani/csv-codec/blob/main/license.md"
  )
)

ThisBuild / homepage := Some(url("https://csv-codec.bilal-fazlani.com/"))

lazy val root = project
  .in(file("."))
  .settings(
    name         := "csv-codec",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "com.softwaremill.magnolia1_3" %% "magnolia" % "1.3.0",
      "org.scalameta"                %% "munit"    % "1.0.0-M7" % Test
    )
  )
