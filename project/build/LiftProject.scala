import sbt._

class LiftProject(info: ProjectInfo) extends DefaultWebProject(info) with IdeaProject {
  val liftVersion = property[Version]

  // uncomment the following if you want to use the snapshot repo
  //  val scalatoolsSnapshot = ScalaToolsSnapshots

  // If you're using JRebel for Lift development, uncomment
  // this line
  // override def scanDirectories = Nil

  val MavenLocal = "Local Maven Repository" at "file://" + Path.userHome + "/.m2/repository"
  
  lazy val JavaNet = "Java.net Maven2 Repository" at "http://download.java.net/maven/2/"

  val SonatypeRep = "Sonatype scala-tools repo" at "https://oss.sonatype.org/content/groups/scala-tools/"

  override def libraryDependencies = Set(
    "net.liftweb" %% "lift-webkit" % liftVersion.value.toString % "compile",
    "net.liftweb" %% "lift-mapper" % liftVersion.value.toString % "compile",
    "org.mortbay.jetty" % "jetty" % "6.1.26",
    "io.netty" % "netty" % "3.4.4.Final", 
    "joda-time" % "joda-time" % "2.1", 
    "org.joda" % "joda-convert" % "1.2", 
    "junit" % "junit" % "4.7" % "test",
    "ch.qos.logback" % "logback-classic" % "0.9.26",
    "org.scala-tools.testing" %% "specs" % "1.6.9" % "test",
    "com.h2database" % "h2" % "1.2.147"
  ) ++ super.libraryDependencies
}
