package ru.ifmo.rain.kokorin.lexer

import java.io.{IOException, Reader}

import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

class Lexer (readerGetter: => Reader) extends AutoCloseable {
  private val reader = readerGetter
  private var curChar = -1337

  private def readNextChar() {
    curChar = reader.read()
  }

  readNextChar()

  private def isLetter(x: Char)= ('a' <= x && x <= 'z') || ('A' <= x && x <= 'Z')

  @throws(classOf[LexingException])
  def getNextToken(): Token = Try {
    while (curChar != -1 && Character.isWhitespace(curChar.toChar)) {
      readNextChar()
    }
    if (curChar == -1) {
      End
    } else {
      curChar.toChar match {
        case '*' =>
          readNextChar()
          Asterisk

        case '(' =>
          readNextChar()
          LeftParent

        case ')' =>
          readNextChar()
          RightParent

        case ',' =>
          readNextChar()
          Comma

        case ';' =>
          readNextChar()
          Semi

        case x if isLetter(x) =>
          val result = StringBuilder.newBuilder.append(x)
          while (curChar != -1 && isLetter(curChar.toChar)) {
            readNextChar()
            if (isLetter(curChar.toChar)) {
              result.append(curChar.toChar)
            }
          }
          Word(result.toString())

        case x => throw new LexingException(s"Unknown character $x")
      }
    }
  } match {
    case Failure(exception) if exception.isInstanceOf[IOException] =>
      throw new LexingException("IO exception while reading next token", exception)

    case Failure(exception) if exception.isInstanceOf[LexingException] =>
      throw exception

    case Failure(exception) if NonFatal(exception) =>
      throw new LexingException(exception)

    case Success(value) =>
      value
  }

  @throws(classOf[IOException])
  override def close(): Unit = {
    reader.close()
  }
}
