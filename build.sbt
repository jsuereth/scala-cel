resolvers += ScalaToolsSnapshots

organization := "org.scala-lang.cel"

version := "cel-1.0-SNAPSHOT"

scalaVersion := "2.10.0-SNAPSHOT"

scalacOptions in Compile ++= Seq("-deprecation", "-unchecked")
