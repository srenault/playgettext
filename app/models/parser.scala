package models

import scala.util.parsing.input._
import scala.util.parsing.input.Positional
import scala.util.parsing.combinator._
import scala.util.matching._

case class Message(key: String, pattern: String, input: scalax.io.Input, sourceName: String) extends Positional

class MessagesParser(messageInput: scalax.io.Input, messageSourceName: String) extends RegexParsers {
  override def skipWhitespace = false

  def namedError[A](p: Parser[A], msg: String) = Parser[A] { i =>
    p(i) match {
      case Failure(_, in) => Failure(msg, in)
      case o => o
    }
  }

  def end = namedError("""\s*""".r, "End of line expected")

  def newLine = namedError((("\r"?) ~> "\n"), "End of line expected")

  def quote = namedError("\"", "Quote expected")

  def msgId = namedError("""(.[^"]+)""".r, "msgid expected")

  def msgPattern = namedError("""(.[^"]+)""".r, "Message pattern expected")

  def message = "msgid" ~ whiteSpace ~ quote ~ msgId ~ quote ~ newLine ~ "msgstr" ~ whiteSpace ~ quote ~ msgPattern ~ quote ^^ {
    case (_ ~ _ ~ _ ~ key ~ _ ~ _ ~ _ ~ _ ~ _ ~ value ~ _) => {
      Message(key, value, messageInput, messageSourceName)
    }
  }

  def parser = phrase((message*) <~ end) ^^ {
    case messages => messages.collect {
      case m @ Message(_, _, _, _) => m
    }
  }

  def parse = {
    parser(new CharSequenceReader(messageInput.string + "\n")) match {
      case Success(messages, _) => messages
      case NoSuccess(message, in) => throw new Exception(message)
    }
  }
}
