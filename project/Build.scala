import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "gettext"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
    // Add your project dependencies here,
    jdbc,
    anorm
  )

  // Tasks
  val gettext = TaskKey[Unit]("gettext", "Generate Ressource Bundle classes from PO files for i18n")

  val gettextTask = gettext <<= (baseDirectory, streams) map { (base, s) =>
    s.log.info("Generating Ressource Bundle classes from the following PO files :")
    s.log.info(IO.listFiles(base / "conf" / "po").map(_.getName:String).toList.mkString("\n"))
  }

  val main = play.Project(appName, appVersion, appDependencies, settings = Seq(gettextTask) ++ Defaults.defaultSettings).settings(
    // Add your own project settings here      
  )
}
