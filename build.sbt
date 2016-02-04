name := "interval-tree"

version := "0.1-SNAPSHOT"

organization := "com.github.erictu"

scalaVersion := "2.10.4"

publishMavenStyle := true
resolvers += Resolver.mavenLocal

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishArtifact in Test := false
pomIncludeRepository := { _ => false }

pomExtra := (
  <url>http://github.com/erictu/interval-tree</url>
  <scm>
    <url>git@github.com:erictu/interval-tree.git</url>
    <connection>scm:git:git@github.com:erictu/interval-tree.git</connection>
  </scm>
  <developers>
    <developer>
      <id>erictu</id>
      <name>Eric Tu</name>
      <url>http://github.com/erictu</url>
    </developer>
    <developer>
      <id>akmorrow13</id>
      <name>Alyssa Morrow</name>
      <url>http://github.com/akmorrow13</url>
    </developer>
  </developers>)

licenses += "Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0.html")

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.2" % "test"

libraryDependencies += "org.bdgenomics.adam" %% "adam-cli" % "0.18.3-SNAPSHOT"
libraryDependencies += "org.bdgenomics.adam" %% "adam-core" % "0.18.3-SNAPSHOT"
libraryDependencies += "org.bdgenomics.adam" %% "adam-core" % "0.18.3-SNAPSHOT" % "test" classifier "tests"

// Run tests with more memory
javaOptions in test += "-Xmx2G"

fork in test := true

javaOptions += "-Xmx10G"
