import sbt._
import Keys._
import Load.BuildStructure


trait CelProjects {

  lazy val scalaArm = uri("git://github.com/jsuereth/scala-arm.git")
  lazy val scalaCheck = uri("git://github.com/rickynils/scalacheck.git")
  // Scalaz is needed for specs.
  lazy val scalaz = uri("git://github.com/etorreborre/scalaz.git#scala-2.9.x")
  lazy val specs2 = uri("git://github.com/etorreborre/specs2.git")
  lazy val antixml = uri("git://github.com/djspiewak/anti-xml.git")
  //lazy val scalaIo = uri("git://github.com/scala-incubator/scala-io.git")

  // Scala-cel project refs in dependency order.   Note:  Builds will be performed in the order of this
  // sequence.
  lazy val projectRefs: Seq[ProjectReference] = Seq(scalaArm, scalaCheck, scalaz, specs2, antixml)
}

case class MyDependencyInfo(project: ProjectRef,
                            name: String, 
                            organization: String, 
                            version: String, 
                            module: ModuleID,
                            dependencies: Seq[ModuleID] = Seq())

case class MyDependencyActions(info: MyDependencyInfo,
                               addProjectDependency: Seq[ModuleID] = Nil,
                               removeLibraryDependency: Seq[ModuleID] = Nil)

case class CelInfo(version: String, organization: String, scalaVersion: String)
                               
trait DependencyAnalysis {
  def hashInfo(d: MyDependencyInfo) = hashSynonym(d.organization + ":" + d.name)
  def hashModule(o: ModuleID) = hashSynonym(o.organization + ":" + o.name)
  def hashSynonym(x: String) = x match {
   //case "org.specs2:specs2-scalaz-core" => "org.scalaz:scalaz"
   case x => x
  }

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
  def analyseDependencies(results: Vector[MyDependencyInfo], info: CelInfo): Seq[MyDependencyActions] = {
    val lookUp = (Map[String, MyDependencyInfo]() /: results) { (m, value) =>
       m + (hashInfo(value) -> value)
    }
    (results map { value =>
      val changes = for { dep <- value.dependencies
        proj <- lookUp.get(hashModule(dep))
      } yield (dep.copy(organization=info.organization, revision=info.version), dep)
      MyDependencyActions(value, changes map (_._1), changes map (_._2))
    } 
    filterNot (_.addProjectDependency.isEmpty) 
    filterNot (_.removeLibraryDependency.isEmpty))
  }
  /** Transforms a set of settings so that the CEL build will succeed. */
  def fixCellProjectSettings(actions: Seq[MyDependencyActions], info: CelInfo)(settings: Seq[Setting[_]]) = {
    def fixLibraryDependencies(s: Setting[_]): Setting[_] = if(s.key.scope.project.isInstanceOf[Select[_]]) s.asInstanceOf[Setting[Seq[ModuleID]]] mapInit { (_, old) =>        
      val Scope(Select(ref), _, _, _) = s.key.scope
      val toRemove = (for {
        proj <- actions
        if proj.info.project == ref
        dep <- proj.removeLibraryDependency
      } yield dep).toSet
      val toAdd = (for {
        proj <- actions
        if proj.info.project == ref
        dep <- proj.addProjectDependency
      } yield dep)
      (old filterNot toRemove) ++ toAdd
      // TODO - Add new dependencies!
    } else s
    // Now fix dependencies
    def f(s: Setting[_]): Setting[_] = s.key.key match {
      // TODO - Create a *real* CEL version
      case version.key             => s.asInstanceOf[Setting[String]].mapInit((_,_) => info.version)
      case organization.key        => s.asInstanceOf[Setting[String]].mapInit( (_,_) => info.organization)
      case scalaVersion.key        => s.asInstanceOf[Setting[String]].mapInit((_,_) => info.scalaVersion)
      case resolvers.key           => s.asInstanceOf[Setting[Seq[Resolver]]].mapInit((_,old) => (old :+ ScalaToolsSnapshots).toSet.toSeq)
      case libraryDependencies.key => fixLibraryDependencies(s)
      case _ => s
    } 
    settings map f
  }
}
                               
object CommunityExtensionsBuild extends Build with DependencyAnalysis with CelProjects {
  //val info = CelInfo("cel-1.0-SNAPSHOT", "org.scala-lang.cel", "2.9.1")
  lazy val celFixed = AttributeKey[Boolean]("scala-cel-references-fixed")
  lazy val root = Project("root", file(".")) dependsOn(projectDeps: _*) settings(
    commands += celSetup,
    commands += celTest,
    onLoad in Global <<= (onLoad in Global) ?? idFun[State],
    onLoad in Global <<= (onLoad in Global) apply ( _ andThen ("cel-setup" :: _))
  )
  
  lazy val projectDeps: Seq[ClasspathDependency] = projectRefs map (new ClasspathDependency(_, None))

  def celTest = Command.command("cel-test") { (state: State) =>
    val extracted = Project.extract(state)
    import extracted._ 
    val magikTasks = Seq(test in Test, publishLocal)
    for(ref <- projectRefs; task <- magikTasks) {
      // TODO - Better print statements
      println("Executing " + task.key + " in " + ref)
      Project.evaluateTask(task in ref, state)
    }
    state
  }
 
  /** Pulls the name/organization/version for each project in the CEL build */
  def getCelInfo(extracted: Extracted): CelInfo =
    CelInfo(extracted.get(Keys.version in root),
            extracted.get(Keys.organization in root),
            extracted.get(Keys.scalaVersion in root))

 
  // Define the command.  This takes the existing settings (including any session settings)
  // and applies 'f' to each Setting[_]
  def celSetup = Command.command("cel-setup") { (state: State) =>
    if(state.get(celFixed) getOrElse false) state
    else {
    // TODO - Don't run if already run.
      val extracted = Project.extract(state)
      import extracted._ 
      // Bump Scala version and build projects.
      val info = getCelInfo(extracted)
      val refs = (session.mergeSettings map (_.key.scope) collect {
        case Scope(Select(p @ ProjectRef(_,_)),_,_,_) => p
      } toSet)
      val projectInfos = getProjectInfos(extracted, refs)
      val projectActions = analyseDependencies(projectInfos, info)
      val transformedSettings = fixCellProjectSettings(projectActions, info)(session.mergeSettings)
      // Now we need to rip into structure and add references to appropriate projects.
      import Load._      
      val newStructure2 = Load.reapply(transformedSettings, structure)
      Project.setProject(session, newStructure2, state).put(celFixed, true)
    }
  }
}
