ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.8.2"

lazy val root = (project in file("."))
  .settings(
    name := "tda4s",
    idePackagePrefix := Some("org.appliedtopology.tda4s")
  )

// Add ScalaTest and ScalaCheck dependencies
lazy val scalatestVersion = "3.2.17" // Or the latest compatible version
lazy val scalacheckVersion = "1.17.0" // Or the latest compatible version

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % scalatestVersion % Test,
  "org.scalatestplus" %% "scalacheck-1-17" % s"$scalatestVersion.0" % Test
)
