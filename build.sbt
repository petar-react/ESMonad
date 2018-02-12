name := "ESMonad"

version := "0.1"

scalaVersion := "2.12.4"

lazy val root = (project in file(".")).settings(
  inThisBuild(
    List(
      organization := "org.com.lambdaworks",
      scalaVersion := "2.12.4",
      version := "0.1.0-SNAPSHOT"
    )),
  name := "ESMonad",
  scalacOptions += "-feature",
  scalacOptions += "-language:postfixOps",
  scalacOptions += "-language:higherKinds",
  scalacOptions += "-Ypartial-unification",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.3" % Test,
  libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.1",
  libraryDependencies += "org.typelevel" %% "cats-free" % "1.0.1",
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
)
