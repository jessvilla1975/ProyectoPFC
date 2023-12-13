ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.12"

lazy val root = (project in file("."))
  .settings(
    name := "scala3-playground",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.9" % Test,
      "org.scalatestplus" %% "junit-4-13" % "3.2.9.0" % Test,
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.3",
      "com.storm-enroute" %% "scalameter" % "0.20"
    )
  )



