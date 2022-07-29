name := "living-with-cats"

version := "0.1"

scalaVersion := "2.13.8"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.7.0"
scalacOptions ++= Seq("-Xfatal-warnings")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % Test
