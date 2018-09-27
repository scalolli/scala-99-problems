import Dependencies._

lazy val ScalaCheck = Seq("org.scalacheck"          %% "scalacheck"        % "1.14.0").map(_ % Test)

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.6",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "scala-99-problems",
    libraryDependencies ++= Seq(scalaTest % Test) ++ ScalaCheck
  )
