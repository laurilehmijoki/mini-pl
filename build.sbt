lazy val root = (project in file("."))
  .settings(
    name         := "mini-pl",
    organization := "experimental",
    scalaVersion := "2.12.2",
    version      := "0.1.0-SNAPSHOT"
  )

libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.8.9" % "test")

scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xfatal-warnings")

scalacOptions in Test ++= Seq("-Yrangepos")
