name := "eff-test"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies += "org.atnos" %% "eff" % "2.0.2"

scalacOptions ++= Seq("-Ypartial-unification")
    