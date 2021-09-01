val scala3Version = "3.0.1"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "csv-codec",
    version      := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.typelevel" %% "shapeless3-deriving" % "3.0.2",
      "org.scalameta" %% "munit"               % "0.7.27" % Test
    )
  )
