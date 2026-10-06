name := "pcd-assignment-03"

version := "1.0"

scalaVersion := "3.3.1"
lazy val akkaVersion = "2.8.5"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion,
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "ch.qos.logback" % "logback-classic" % "1.5.18",
  "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
)

fork := true
