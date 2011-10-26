resolvers += ScalaToolsSnapshots

organization := "org.scala-lang.cel"

version := "cel-1.0-SNAPSHOT"

scalaVersion := "2.9.1"

scalacOptions in Compile ++= Seq("-deprecation", "-unchecked")
