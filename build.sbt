name := "uqbar-math"

description := "A math extension for Scala with better abstractions to support complex algebraic expressions"

scalaVersion := "2.11.1"

///////////////////////////////////////////////////////////////////////////////////////////////////
// PROJECT SETTINGS

libraryDependencies += "org.scalatest" %% "scalatest" % "[2.2,)" % "test"

unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value)

unmanagedSourceDirectories in Test := Seq((scalaSource in Test).value)

scalacOptions += "-feature"

///////////////////////////////////////////////////////////////////////////////////////////////////
// ECLIPSE SETTINGS

EclipseKeys.createSrc := EclipseCreateSrc.Default

EclipseKeys.withSource := true

///////////////////////////////////////////////////////////////////////////////////////////////////
// PUBLISHING SETTINGS

releaseSettings

crossScalaVersions := Seq(scalaVersion.value)

publishTo := Some(Resolver.file("Local Maven Repository",  file(Path.userHome.absolutePath + "/.m2/repository")))