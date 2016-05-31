lazy val root = (project in file(".")).
  settings(
    organization := "net.nomadicalien",
    name := "ai-nlp",
    version := "1.0-SNAPSHOT",
    scalaVersion := "2.11.8"
  )

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.4.6",
  "com.typesafe.akka" %% "akka-stream" % "2.4.6",
  "org.apache.logging.log4j" % "log4j-api" % "2.1",
  "org.apache.logging.log4j" % "log4j-core" % "2.1",
  "org.specs2" %% "specs2-core" % "3.8.3" % Test
)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-Xlint")

scalacOptions in Test ++= Seq("-Yrangepos", "-Xlint")

assemblyJarName in assembly := "ai-nlp-with-deps.jar"

mainClass in assembly := Some("net.nomadicalien.nlp.App")

addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1.14")