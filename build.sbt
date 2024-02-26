ThisBuild / scalaVersion     := "2.13.12"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

enablePlugins(JmhPlugin)

lazy val root = (project in file("."))
  .settings(
    name := "scala-zio-playground",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "2.0.19",
      "dev.zio" %% "zio-test" % "2.0.19" % Test,
      "org.foundationdb" % "fdb-java" % "7.3.31"
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )
