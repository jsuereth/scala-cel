import sbt._
import Keys._
import Load.BuildStructure

object CommunityExtensionsBuild extends Build {
  lazy val root = Project("root", file(".")) dependsOn(projectDeps: _*) settings(
    commands += magikCommand("cel-test", Seq(compile in Compile, test in Test))
  )

  lazy val scalaArm = uri("git://github.com/jsuereth/scala-arm.git")
  lazy val scalaCheck = uri("git://github.com/rickynils/scalacheck.git")
  lazy val specs2 = uri("git://github.com/etorreborre/specs2.git")
  //lazy val scalaIo = uri("git://github.com/scala-incubator/scala-io.git")

  // Scala-cel project refs in dependency order.   Note:  Builds will be performed in the order of this
  // sequence.
  lazy val projectRefs: Seq[ProjectReference] = Seq(scalaArm, scalaCheck /*, specs2*/ )
  lazy val projectDeps: Seq[ClasspathDependency] = projectRefs map (new ClasspathDependency(_, None))


  // Define the command.  This takes the existing settings (including any session settings)
  // and applies 'f' to each Setting[_]
  def magikCommand(name: String, magikTasks: Seq[ScopedTask[_]]) = Command.command(name) { (state: State) =>
    val extracted = Project.extract(state)
    import extracted._

    println("ZOOOOOOOOMMMMMMMGGGG!")


    // Bump Scala version and build projects.
    def f(s: Setting[_]): Setting[_] = s.key.key match {
      // TODO - Create a *real* CEL version
      case version.key => 
        s.asInstanceOf[Setting[String]].mapInit( (_,_) => "cel-1.0-SNAPSHOT")
      //case organization.key =>
      //  s.asInstanceOf[Setting[String]].mapInit( (_,_) => "org.scala-lang.cel")
      case scalaVersion.key =>
          s.asInstanceOf[Setting[String]].mapInit((_,_) => "2.10.0-SNAPSHOT")
      case resolvers.key =>
          s.asInstanceOf[Setting[Seq[Resolver]]].mapInit((_,old) => (old :+ ScalaToolsSnapshots).toSet.toSeq)
      // TODO - Update inter-project dependencies so that they reference each other!
      case _ => s
    }
    val transformed = session.mergeSettings map ( s => f(s) )
    val newStructure = Load.reapply(transformed, structure)
    val newState = Project.setProject(session, newStructure, state)
    //Project.evaluateTask(test in scalaArm, newState)
    for(ref <- projectRefs; task <- magikTasks) {
      // TODO - Better print statements
      println("Executing " + task.key + " in " + ref)
      Project.evaluateTask(task in ref, newState)
    }
    // Find all the dependent projects.
    newState
  }

}
