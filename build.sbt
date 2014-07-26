name := "uqbar-math"

organization := "org.uqbar"

scalaVersion := "2.11.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "[2.2,)" % "test"

///////////////////////////////////////////////////////////////////////////////////////////////////
// PROJECT SETTINGS

unmanagedSourceDirectories in Compile := Seq((scalaSource in Compile).value)

unmanagedSourceDirectories in Test := Seq((scalaSource in Test).value)

///////////////////////////////////////////////////////////////////////////////////////////////////
// ECLIPSE SETTINGS

EclipseKeys.createSrc := EclipseCreateSrc.Default

EclipseKeys.withSource := true

///////////////////////////////////////////////////////////////////////////////////////////////////
// PUBLISHING SETTINGS

releaseSettings

publishTo := Some(Resolver.file("Local Maven Repository",  file(Path.userHome.absolutePath + "/.m2/repository")))