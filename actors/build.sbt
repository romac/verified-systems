
name := "actors"

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.11.8"

unmanagedSourceDirectories in Compile += baseDirectory.value / "extra-src"

scalacOptions ++= Seq("-Xdisable-assertions")

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.5.6",
  "com.typesafe.akka" %% "akka-testkit" % "2.5.6" % Test
)
