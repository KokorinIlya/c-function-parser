package ru.ifmo.rain.kokorin.parser

import java.io.{IOException, Reader}

import ru.ifmo.rain.kokorin.lexer._

import scala.util.control.NonFatal

class Parser (val lexer: Lexer) extends AutoCloseable {

  private var curToken: Token = _

  private def parseS(): S = {
    curToken match {
      case Word(_) =>
        val t = parseT()

        val name = curToken match {
          case x: Word => x

          case y => throw new ParseException(s"Expected function name, received $y")
        }

        curToken = lexer.getNextToken()
        val leftPar = curToken match {
          case LeftParent => LeftParent

          case y => throw new ParseException(s"Expected left parenthesis, received $y")
        }

        curToken = lexer.getNextToken()
        val a = parseA()

        val rightPar = curToken match {
          case RightParent => RightParent

          case y => throw new ParseException(s"Expected right parenthesis, received $y")
        }

        val semi = lexer.getNextToken() match {
          case Semi => Semi

          case y => throw new ParseException(s"Expected ';' token, received $y")
        }

        curToken = lexer.getNextToken()
        S(t :: name.toTree :: leftPar.toTree :: a :: rightPar.toTree :: semi.toTree :: Nil)

      case y => throw new ParseException(s"Parsing S, expecting {Word}, got $y")
    }
  }

  private def parseT(): T = {
    curToken match {
      case Word(_) =>
        val typeName = curToken
        curToken = lexer.getNextToken()
        val tPrime = parseTPrime()
        T(typeName.toTree :: tPrime :: Nil)

      case y => throw new ParseException(s"Parsing T, expecting {Word}, got $y")
    }
  }

  private def parseTPrime(): TPrime = {
    curToken match {
      case Word(_) =>
        TPrime()

      case Asterisk =>
        curToken = lexer.getNextToken()
        val tPrime = parseTPrime()
        TPrime(Asterisk.toTree :: tPrime :: Nil)

      case y => throw new ParseException(s"Parsing T', expecting {Word | *}, got $y")
    }
  }

  private def parseA(): A = {
    curToken match {
      case RightParent =>
        A()

      case Word(_) =>
        val b = parseB()
        val aPrime = parseAPrime()
        A(b :: aPrime :: Nil)

      case y => throw new ParseException(s"Parsing A, expecting {Word | )}, got $y")
    }
  }

  private def parseAPrime(): APrime = {
    curToken match {
      case Comma =>
        curToken = lexer.getNextToken()
        val a = parseA()
        APrime(Comma.toTree :: a :: Nil)

      case RightParent =>
        APrime()

      case y => throw new ParseException(s"Parsing A', expecting {, | )}, got $y")
    }
  }

  private def parseB(): B = {
    curToken match {
      case Word(_) =>
        val t = parseT()
        val word = curToken
        curToken = lexer.getNextToken()

        B(t :: word.toTree :: Nil)

      case y => throw new ParseException(s"Parsing B', expecting {Word}, got $y")
    }
  }

  @throws(classOf[ParseException])
  @throws(classOf[IOException])
  def parse(): S = {
    try {
      curToken = lexer.getNextToken()
      val s = parseS()
      val _ = curToken match {
        case End => End

        case y => throw new ParseException(s"Expected end of declaration, received $y")
      }
      s
    } catch {
      case ex: LexingException => throw new ParseException("Lexer error", ex)

      case ex if NonFatal(ex) => throw new ParseException(ex)
    } finally {
      lexer.close()
    }
  }

  @throws(classOf[IOException])
  override def close(): Unit = {
    lexer.close()
  }
}

object Parser {
  def apply(reader: => Reader) = new Parser(new Lexer(reader))

  @throws(classOf[ParseException])
  @throws(classOf[IOException])
  def parse(reader: => Reader): S = Parser(reader).parse()
}
