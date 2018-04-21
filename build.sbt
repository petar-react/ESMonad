import sbt.CrossVersion

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
  libraryDependencies ++= Seq(
    "com.github.mpilquist" %% "simulacrum" % "0.11.0",
    "com.chuusai" %% "shapeless" % "2.3.3",
    "com.fommil" %% "deriving-macro" % "0.9.0",
    "org.apache.commons" % "commons-lang3" % "3.0",
    "org.typelevel" %% "cats-core" % "1.0.1",
    "org.typelevel" %% "cats-free" % "1.0.1",
    "io.monix" %% "monix" % "3.0.0-RC1",
    "org.redisson" % "redisson" % "3.6.4",
    "com.typesafe.slick" %% "slick" % "3.2.2",
    "com.h2database" % "h2" % "1.4.185",
    "ch.qos.logback" % "logback-classic" % "1.2.3",
    "org.scalatest" %% "scalatest" % "3.0.3" % Test
  ),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4"),
  addCompilerPlugin(
    "org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full
  )
)
