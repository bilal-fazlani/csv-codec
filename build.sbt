val scala3Version = "3.2.2"

lazy val root = project
  .in(file("."))
  .settings(
    name         := "csv-codec",
    version      := "0.2.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "com.softwaremill.magnolia1_3" %% "magnolia" % "1.3.0",
      "org.scalameta"                %% "munit"    % "1.0.0-M7" % Test
    )
  )
