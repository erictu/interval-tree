name := "interval-tree"

version := "0.1.0-SNAPSHOT"

organization := "com.github.akmorrow13"

scalaVersion := "2.10.4"

publishMavenStyle := true

licenses += "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.html")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"

// Run tests with more memory
javaOptions in test += "-Xmx2G"

fork in test := true
