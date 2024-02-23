ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "cpu_scala",
    idePackagePrefix := Some("org.kr.cpu")
  )

libraryDependencies ++= Seq (
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.3.0",
  ("io.github.krzys9876" %% "command-line-reader" % "1.1.0").cross(CrossVersion.for3Use2_13),
  "org.scalatest" %% "scalatest" % "3.2.15" % Test,
  "org.scalatestplus" %% "scalacheck-1-17" % "3.2.16.0" % Test
)
