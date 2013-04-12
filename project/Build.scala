import sbt._
import Keys._
import play.Project._

object ApplicationBuild extends Build {

  val appName         = "gettext"
  val appVersion      = "1.0-SNAPSHOT"

  val appDependencies = Seq(
//    "com.ibm.icu" % "icu4j" % "4.0.1"
  )

  // Tasks
  val gettext = TaskKey[Unit]("gettext", "Generate Ressource Bundle classes from PO files for i18n")

  val gettextTask = gettext <<= (baseDirectory, streams) map { (base, s) =>
    import scala.sys.process._

    s.log.info("Generating Ressource Bundle classes from :")
    val poDirectory = (base / "conf" / "po")
    val poFiles = IO.listFiles(poDirectory)
    s.log.info(poFiles.map(_.getName:String).mkString("\n"))

    val targetDirectory = (base / "target" / "scala-2.10" / "classes").getCanonicalPath
    poFiles foreach { po =>
      val poPath = po.getCanonicalPath
      val lang = po.getName.split("\\.").headOption
      lang.map { lg =>
        val cmd = "msgfmt --java -d %s -r playgettext.i18n.Messages -l %s %s".format(targetDirectory, lg, poPath)
        if((Process(cmd) ! ProcessLogger(out => (), err => s.log.error(err))) != 0) sys.error("Can't generate Ressource Bundle Class for: " + po.getCanonicalPath)
      } getOrElse {
        s.log.warn("The following PO file is ignored: " + po.getCanonicalPath)
      }
    }
  }

  val main = play.Project(appName, appVersion, appDependencies, settings = Seq(gettextTask) ++ Defaults.defaultSettings).settings(
    // Add your own project settings here      
  )
}
