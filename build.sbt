import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.gaia",
      scalaVersion := "2.11.12", //TODO: cross compile 2.12.4
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Bones",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-free" % "1.0.1",
      "com.chuusai" %% "shapeless" % "2.3.3",
      "net.liftweb" %% "lift-json" % "2.6.3",
      "org.scalatest" %% "scalatest" % "3.0.4" % Test
    )
  )

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
