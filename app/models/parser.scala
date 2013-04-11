package models

import scala.util.parsing.input._
import scala.util.parsing.input.Positional
import scala.util.parsing.combinator._
import scala.util.matching._

case class Message(key: String, pattern: String, input: scalax.io.Input, sourceName: String) extends Positional
case class Comment(msg: String)

class MessagesParser(messageInput: scalax.io.Input, messageSourceName: String) extends RegexParsers {

  override def skipWhitespace = false

  def namedError[A](p: Parser[A], msg: String) = Parser[A] { i =>
    p(i) match {
      case Failure(_, in) => Failure(msg, in)
      case o => o
    }
  }

  def end = namedError("""\s*""".r, "End of file expected")

  def newLine = namedError((("\r"?) ~> "\n"), "End of line expected")

  def blankLine = opt(whiteSpace) ~> newLine ^^ { case _ => Comment("") }

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

  def sentence = (header | comment | positioned(message))

  def parser = phrase((sentence | newLine)*) ^^ {
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
