import sbt._
import Keys._
import Load.BuildStructure


case class MyDependencyInfo(project: ProjectRef,
                            name: String, 
                            organization: String, 
                            version: String, 
                            module: ModuleID,
                            dependencies: Seq[ModuleID] = Seq())

case class MyDependencyActions(info: MyDependencyInfo,
                               addProjectDependency: Seq[ProjectRef] = Nil,
                               removeLibraryDependency: Seq[ModuleID] = Nil)

object CommunityExtensionsBuild extends Build {
  lazy val celFixed = AttributeKey[Boolean]("scala-cel-references-fixed")
  lazy val root = Project("root", file(".")) dependsOn(projectDeps: _*) settings(
    commands += magikCommand("cel-setup"),
    onLoad in Global <<= (onLoad in Global) ?? idFun[State],
    onLoad in Global <<= (onLoad in Global) apply ( _ andThen ("cel-setup" :: _))
  )
  lazy val scalaArm = uri("git://github.com/jsuereth/scala-arm.git")
  lazy val scalaCheck = uri("git://github.com/rickynils/scalacheck.git")
  lazy val specs2 = uri("git://github.com/etorreborre/specs2.git")
  //lazy val scalaIo = uri("git://github.com/scala-incubator/scala-io.git")

  // Scala-cel project refs in dependency order.   Note:  Builds will be performed in the order of this
  // sequence.
  lazy val projectRefs: Seq[ProjectReference] = Seq(scalaArm, scalaCheck, specs2)
  lazy val projectDeps: Seq[ClasspathDependency] = projectRefs map (new ClasspathDependency(_, None))

  def hashInfo(d: MyDependencyInfo) = d.organization + ":" + d.name
  def hashModule(o: ModuleID) = o.organization + ":" + o.name

  /** Pulls the name/organization/version for each project in the CEL build */
  def getProjectInfos(extracted: Extracted, refs: Iterable[ProjectRef]) =
    (Vector[MyDependencyInfo]() /: refs) { (dependencies, ref) =>
      dependencies :+ MyDependencyInfo(
        ref,
        extracted.get(Keys.name in ref),
        extracted.get(Keys.organization in ref),
        extracted.get(Keys.version in ref),
        extracted.get(Keys.projectID in ref),
        extracted.get(Keys.libraryDependencies in ref))
    }

  /** Figures out which libraryDependencies on a project should be moved to project dependencies. */
  def analyseDependencies(results: Vector[MyDependencyInfo]): Seq[MyDependencyActions] = {
    val lookUp = (Map[String, MyDependencyInfo]() /: results) { (m, value) =>
       m + (hashInfo(value) -> value)
    }
    println("---- Dependencies = " + lookUp.mkString("\n\t", "\n\t", "\n"))
    (results map { value =>
      val changes = for { dep <- value.dependencies
        proj <- lookUp.get(hashModule(dep))
      } yield (proj.project, dep)
      // TODO - If we change the groupIds, we need to change the project moduleID group ids...
      MyDependencyActions(value, changes map (_._1), changes map (_._2))
    } 
    filterNot (_.addProjectDependency.isEmpty) 
    filterNot (_.removeLibraryDependency.isEmpty))
  }
  

  // Define the command.  This takes the existing settings (including any session settings)
  // and applies 'f' to each Setting[_]
  def magikCommand(name: String) = Command.command(name) { (state: State) =>
    if(state.get(celFixed) getOrElse false) state
    else {
    // TODO - Don't run if already run.
      val extracted = Project.extract(state)
      import extracted._ 
      // Bump Scala version and build projects.
      val refs = (session.mergeSettings map (_.key.scope) collect {
        case Scope(Select(p @ ProjectRef(_,_)),_,_,_) => p
      } toSet)
      val projectInfos = getProjectInfos(extracted, refs)
      val projectActions = analyseDependencies(projectInfos)
      def fixLibraryDependencies(s: Setting[_]): Setting[_] = if(s.key.scope.project.isInstanceOf[Select[_]]) s.asInstanceOf[Setting[Seq[ModuleID]]] mapInit { (_, old) =>        
        val Scope(Select(ref), _, _, _) = s.key.scope
        val toRemove = (for {
           proj <- projectActions
           if proj.info.project == ref
           dep <- proj.removeLibraryDependency
        } yield dep).toSet
        old filterNot toRemove
      } else s
      // Now fix dependencies
      def f(s: Setting[_]): Setting[_] = s.key.key match {
        // TODO - Create a *real* CEL version
        case version.key             => s.asInstanceOf[Setting[String]].mapInit((_,_) => "cel-1.0-SNAPSHOT")
        //case organization.key        => s.asInstanceOf[Setting[String]].mapInit( (_,_) => "org.scala-lang.cel")
        case scalaVersion.key        => s.asInstanceOf[Setting[String]].mapInit((_,_) => "2.10.0-SNAPSHOT")
        case resolvers.key           => s.asInstanceOf[Setting[Seq[Resolver]]].mapInit((_,old) => (old :+ ScalaToolsSnapshots).toSet.toSeq)
        case libraryDependencies.key => fixLibraryDependencies(s)
        case _ => s
      }      
      val transformedSettings = session.mergeSettings.map( s => f(s) )
      // Now we need to rip into structure and add references to appropriate projects.
      import Load._
      val projectActionMap = (projectActions collect {
        case a @ MyDependencyActions(MyDependencyInfo(ProjectRef(uri,name),_,_,_,_,_),_,_) => (uri, name) -> a
      } toMap)
      println("Actions map = " + projectActions.mkString("\n\t","\n\t","\n"))
      def transformUnits(units: Map[URI, LoadedBuildUnit]): Map[URI, LoadedBuildUnit] =
        for((uri, b) <- units) yield {
          def transformProjects(projects: Map[String, ResolvedProject]): Map[String, ResolvedProject] = 
            for((name, project) <- projects) yield {   
              val newProject = (
                projectActionMap.get((uri, name)).map{ a => 
                  println("Found project, updating dependencies!")
                  // TODO - Bug in here...
                  Project.resolved(
                    id = project.id,
                    base = project.base,
                    aggregate = project.aggregate,
                    // TODO - keep original project dependencies...
                    dependencies = (a.addProjectDependency.map{i=>ResolvedClasspathDependency(i, None)}),
                    delegates = project.delegates,
                    settings = project.settings,
                    configurations = project.configurations
                  )
                }.getOrElse(project)
              )
              (name,newProject)
            }
          (uri, new LoadedBuildUnit(b.unit, transformProjects(b.defined), b.rootProjects, b.buildSettings))
        }      
      val newStructure = new BuildStructure(transformUnits(structure.units), 
                                            structure.root,
                                            structure.settings,
                                            structure.data,
                                            structure.index,
                                            structure.streams,
                                            structure.delegates,
                                            structure.scopeLocal)
      
      val newStructure2 = Load.reapply(transformedSettings, newStructure)
      Project.setProject(session, newStructure2, state).put(celFixed, true)
    }
  }
}
