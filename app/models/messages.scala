package models

import scala.util.parsing.input._
import scala.util.parsing.input.Positional
import scala.util.parsing.combinator._
import scala.util.matching._
import play.api.i18n.Lang
import scala.collection.JavaConverters._
import scalax.io.JavaConverters._
import scalax.file._

object POMessages {

  trait Parsed
  case class Message(key: String, pattern: String, input: scalax.io.Input, sourceName: String) extends Positional with Parsed
  case class Comment(msg: String) extends Parsed
  case class PluralFormula(count: Int, formula: String) extends Parsed
  object Ignore extends Parsed

  lazy val messages = Plugin.messages

  def apply(key: String, args: Tuple2[String, Any]*)(implicit lang: Lang): String = {
    Plugin.api.translate(key, args) getOrElse key
  }

  // Load messages

  object Plugin {

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

    def translate(key: String, args: Seq[(String, Any)])(implicit lang: Lang): Option[String] = {
      val langsToTry: List[Lang] = List(lang, Lang(lang.language, ""), Lang("default", ""))
      val pattern: Option[String] = langsToTry.foldLeft[Option[String]](None) { (res, lang) =>
        res.orElse(messages.get(lang.code).flatMap(_.get(key)))
      }
      pattern.map { pattern =>
        val javaArgs = args.map { case (key, value) =>
            key -> value.asInstanceOf[java.lang.Object]
        }.toMap.asJava
        new MessageFormat(pattern, lang.toLocale).format(javaArgs)
      }
    }
  }

  // PO Parser

  class MessagesParser(messageInput: scalax.io.Input, messageSourceName: String) extends RegexParsers {
    override def skipWhitespace = false

    def newLine = ("\r"?) ~> "\n"

    def blankLine = """[(\s)|(\n)]+""".r ^^ { case _ => Ignore }

    def firstTwoLines = """msgid\s""\nmsgstr\s""""".r ^^ { case _ => Ignore }

    def header = """".[^"]+"""".r ^^ { case _ => Ignore }

    def formula = """.[^;]+""".r

    def endFormula = """.[^"]+""".r

    def pluralFormula = """"Plural-Forms:""" ~ whiteSpace ~ "nplurals=" ~ number ~ semicolon ~ whiteSpace ~ "plural=" ~ formula ~ semicolon ~ endFormula ~ quote ^^ {
      case (_ ~ _ ~ _ ~ nb ~ _ ~ _ ~ _ ~ f ~ _ ~ _ ~ _) => {
        println(nb, f)
        PluralFormula(nb.toInt, f)
      }
    }

    def semicolon = ";"

    def comment = """#.*""".r ^^ { case s => Comment(s) }

    def number = """\d""".r

    def quote = "\""

    def msgId = """(.[^"]+)""".r

    def msgPattern = """(.[^"]+)""".r

    def message = "msgid" ~ whiteSpace ~ quote ~ msgId ~ quote ~ newLine ~ "msgstr" ~ whiteSpace ~ quote ~ msgPattern ~ quote ^^ {
      case (_ ~ _ ~ _ ~ key ~ _ ~ _ ~ _ ~ _ ~ _ ~ value ~ _) => {
        Message(key, value, messageInput, messageSourceName)
      }
    }

    def sentence = (firstTwoLines | pluralFormula | header | comment | positioned(message)) <~ newLine

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
