package models

import scala.util.parsing.input._
import scala.util.parsing.input.Positional
import scala.util.parsing.combinator._
import scala.util.matching._
 import play.api.i18n.Lang

object POMessages {

  case class Message(key: String, pattern: String, input: scalax.io.Input, sourceName: String) extends Positional
  case class Comment(msg: String)

  lazy val messages = Plugin.messages

  def apply(key: String, args: Any*)(implicit lang: Lang): String = {
    Plugin.api.translate(key, args) getOrElse key
  }

  // Load messages

  object Plugin {

    import scala.collection.JavaConverters._
    import scalax.file._
    import scalax.io.JavaConverters._

    private def loadMessages(file: String): Map[String, String] = {
      play.api.Play.current.classloader.getResources(file).asScala.toList.reverse.map { messageFile =>
        new MessagesParser(messageFile.asInput, messageFile.toString).parse.map { message =>
          message.key -> message.pattern
        }.toMap
      }.foldLeft(Map.empty[String, String]) { _ ++ _ }
    }

    lazy val messages = {
      MessagesApi {
        Lang.availables(play.api.Play.current).map(_.code).map { lang =>
          (lang, loadMessages("messages." + lang + ".po"))
        }.toMap + ("default" -> loadMessages("messages.po"))
      }
    }

    def api = messages
  }

  // Format Messages

  case class MessagesApi(messages: Map[String, Map[String, String]]) {
    import com.ibm.icu.text.MessageFormat

    def translate(key: String, args: Seq[Any])(implicit lang: Lang): Option[String] = {
      val langsToTry: List[Lang] = List(lang, Lang(lang.language, ""), Lang("default", ""))
      val pattern: Option[String] = langsToTry.foldLeft[Option[String]](None) { (res, lang) =>
        res.orElse(messages.get(lang.code).flatMap(_.get(key)))
      }
      pattern.map { pattern =>
        new MessageFormat(pattern, lang.toLocale).format(args.map(_.asInstanceOf[java.lang.Object]).toArray)
      }
    }
  }

  // PO Parser

  class MessagesParser(messageInput: scalax.io.Input, messageSourceName: String) extends RegexParsers {
    override def skipWhitespace = false

    def namedError[A](p: Parser[A], msg: String) = Parser[A] { i =>
      p(i) match {
        case Failure(_, in) => Failure(msg, in)
        case o => o
      }
    }

    def newLine = namedError((("\r"?) ~> "\n"), "End of line expected")

    def blankLine = """[(\s)|(\n)]+""".r ^^ { case _ => Comment("") }

    def firstTwoLines = """msgid\s""\nmsgstr\s""""".r ^^ { case _ => Comment("") }

    def header = """".[^"]+"""".r ^^ { case _ => Comment("") }

    def comment = """#.*""".r ^^ { case s => Comment(s) }

    def quote = namedError("\"", "Quote expected")

    def msgId = namedError("""(.[^"]+)""".r, "msgid expected")

    def msgPattern = namedError("""(.[^"]+)""".r, "Message pattern expected")

    def message = "msgid" ~ whiteSpace ~ quote ~ msgId ~ quote ~ newLine ~ "msgstr" ~ whiteSpace ~ quote ~ msgPattern ~ quote ^^ {
      case (_ ~ _ ~ _ ~ key ~ _ ~ _ ~ _ ~ _ ~ _ ~ value ~ _) => {
        Message(key, value, messageInput, messageSourceName)
      }
    }

    def sentence = (firstTwoLines | header | comment | positioned(message)) <~ newLine

    def parser = phrase((sentence | blankLine)*) ^^ {
      case messages => messages.collect {
        case m @ Message(_, _, _, _) => m
      }
    }

    def parse = {
      parser(new CharSequenceReader(messageInput.string + "\n")) match {
        case Success(messages, _) => messages
        case NoSuccess(message, in) => {
          throw new play.api.PlayException.ExceptionSource("Configuration error", message) {
            def line = in.pos.line
            def position = in.pos.column - 1
            def input = messageInput.string
            def sourceName = messageSourceName
          }
        }
      }
    }
  }
}
