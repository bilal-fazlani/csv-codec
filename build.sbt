val scala3Version = "3.1.1"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "csv-codec",
    version      := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "com.softwaremill.magnolia1_3" %% "magnolia" % "1.1.1",
      "org.scalameta"                %% "munit"    % "1.0.0-M3" % Test
    )
  )
