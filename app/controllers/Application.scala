package controllers

import play.api._
import play.api.mvc._
import models._

object Application extends Controller {
  def index = Action {
    import scala.collection.JavaConverters._
    import scalax.file._
    import scalax.io.JavaConverters._

    val app = play.api.Play.current

    val res = app.classloader.getResources("po/fr.po").asScala.toList.reverse.map { messageFile =>
      new MessagesParser(messageFile.asInput, messageFile.toString).parse.map { message =>
        message.key -> message.pattern
      }.toMap
    }

    Ok(res.toString)
  }
}
