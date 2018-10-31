package ru.ifmo.rain.kokorin.parser

import play.api.libs.json.{JsArray, JsNumber, JsObject, JsString}
import ru.ifmo.rain.kokorin.lexer.{Asterisk, Comma, Token, Word}

import scala.collection.mutable.ArrayBuffer

sealed class Tree(val description: String, val children: List[Tree]) {
  val symbol: String = this.getClass.getSimpleName

  private def toTreeHelper(builder: StringBuilder){
    builder.append(s"[$description] of type [$symbol]\n")
    for {(child, number) <- children.zipWithIndex} {
      builder.append(s"Child number $number began\n")
      child.toTreeHelper(builder)
      builder.append(s"Child number $number ended\n")
    }
  }

  def toTree: String = {
    val builder = StringBuilder.newBuilder
    toTreeHelper(builder)
    builder.toString()
  }

  private def toStringHelper(buffer: ArrayBuffer[Token]): Unit = {
    this match {
      case TokenHolder(token) =>
        buffer.append(token)

      case _ =>
        for {child <- children} {
          child.toStringHelper(buffer)
        }
    }
  }

  override def toString: String = {
    val buffer = ArrayBuffer[Token]()
    toStringHelper(buffer)
    val tokens = buffer.toArray
    val builder = StringBuilder.newBuilder
    for {index <- 0 until (tokens.length - 1)} {
      val curToken = tokens(index)
      val nextToken = tokens(index + 1)
      builder.append(curToken.toString)
      if ((curToken == Asterisk && nextToken != Asterisk)
        || (curToken.isInstanceOf[Word] && nextToken.isInstanceOf[Word])
        || curToken == Comma) {
        builder.append(" ")
      }
    }
    builder.append(";").toString()
  }

  def toJson: JsObject = {
    val innerMap = this match {
      case TokenHolder(token) =>
        Map(
          "description" -> JsString("token holder"),
          "token" -> JsString(token.toString)
        )

      case _ =>
        Map(
          "description" -> JsString(description),
          "children" -> JsArray(children.map(_.toJson))
        )
    }
    JsObject(innerMap)
  }
}

case class S(override val children: List[Tree]) extends Tree("Function declaration", children)

case class T(override val children: List[Tree]) extends Tree(s"Base part of type", children)

case class TPrime(empty: Boolean, override val children: List[Tree])
  extends Tree(if (empty) "Empty tail of *-part of type" else "Tail of *-part of type", children)

object TPrime {
  def apply(): TPrime = TPrime(empty = true, Nil)

  def apply(children: List[Tree]): TPrime = TPrime(children.isEmpty, children)
}

case class A(empty: Boolean, override val children: List[Tree])
  extends Tree(if (empty) "Empty arguments list" else "Arguments list", children)

object A {
  def apply(): A = A(empty = true, Nil)

  def apply(children: List[Tree]): A = A(children.isEmpty, children)
}

case class APrime(empty: Boolean, override val children: List[Tree])
  extends Tree(if (empty) "Empty tail of arguments list" else "Tail of arguments list", children)

object APrime {
  def apply(): APrime = APrime(empty = true, Nil)

  def apply(children: List[Tree]): APrime = APrime(children.isEmpty, children)
}


case class B(override val children: List[Tree]) extends Tree("Single argument", children)

case class TokenHolder(token: Token) extends Tree(token.toString, Nil)
