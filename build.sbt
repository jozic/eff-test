name := "eff-test"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies += "org.atnos" %% "eff" % "2.2.0"

scalacOptions ++= Seq("-Ypartial-unification")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")