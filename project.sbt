name := "uqbar-math"

description := "A math extension for Scala with better abstractions to support complex algebraic expressions"

scalaVersion := "2.11.6"

///////////////////////////////////////////////////////////////////////////////////////////////////

lazy val uqbarMath = FDProject("org.scalatest" %% "scalatest" % "[2.2,)" % "test")

///////////////////////////////////////////////////////////////////////////////////////////////////

unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value)

unmanagedSourceDirectories in Test := Seq((scalaSource in Test).value)

scalacOptions += "-feature"