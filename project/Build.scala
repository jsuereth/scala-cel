import sbt._
import Keys._

object CommunityExtensionsBuild extends Build {
  lazy val root = Project("root", file(".")) dependsOn(scalaArm)

  lazy val scalaArm = uri("git://github.com/jsuereth/scala-arm.git")
}
