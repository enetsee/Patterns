import sbt._
import sbt.Keys._

object PatternsBuild extends Build {

  lazy val patterns = Project(
    id = "patterns",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "patterns",
      organization := "com.formalconcepts",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.10.0"
      // add other settings here
    )
  )
}
