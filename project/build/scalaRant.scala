import sbt._

class RantProject(info: ProjectInfo) extends DefaultProject(info)
{
  //  override def mainClass = Some("Test")
//  lazy val core = project("core", "Algo for git contest", new CoreProject(_))
  //lazy val ui = project("ui", "Ui", core)
    val releases= "nexus" at "http://shiki:8080/nexus/content/groups/public"
    val specs= "org.scala-tools.testing" % "specs"% "1.5.0" % "test->default"
    val scalacheck = "org.scala-tools.testing" % "scalacheck"% "1.5" %"test->default"
    val scalatest = "org.scala-tools.testing" % "scalatest"% "0.9.5"%"test->default"
    val junit = "junit" % "junit"% "4.6"% "test->default"
    val mockito= "org.mockito" % "mockito-all"% "1.8.0"% "test->default"
}

