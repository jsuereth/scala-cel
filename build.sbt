resolvers += ScalaToolsSnapshots

scalaVersion := "2.10.0-SNAPSHOT"

scalacOptions in Compile ++= Seq("-deprecation", "-unchecked")
