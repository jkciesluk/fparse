name := "fparse"
version := "0.0.1-SNAPSHOT"

scalaVersion := "3.2.2"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.8.0"
libraryDependencies += "com.lihaoyi" %% "pprint" % "0.7.0"
// scalac options come from the sbt-tpolecat plugin so need to set any here

// addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full)
