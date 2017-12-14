
name := "asm"

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.11.8"

scalaSource in Compile := baseDirectory.value / "src"

unmanagedSourceDirectories in Compile += baseDirectory.value / "extra-src"

