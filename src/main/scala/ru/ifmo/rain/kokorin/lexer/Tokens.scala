package ru.ifmo.rain.kokorin.lexer

import ru.ifmo.rain.kokorin.parser.NTerm

sealed trait Token {
  def toTree: NTerm = NTerm(this)
}

object Token {
  def tokensEqual(a: Token, b: Token): Boolean = {
    (a, b) match {
      case (Asterisk, Asterisk) | (LeftParent, LeftParent) | (RightParent, RightParent)
           | (Comma, Comma) | (Semi, Semi) | (End, End) => true

      case (Word(word1), Word(word2)) =>
        val res = word1.equals(word2)
        res

      case _ => false
    }
  }
}

object Asterisk extends Token {
  override def toString: String = "*"
}

object LeftParent extends Token {
  override def toString: String = "("
}

object RightParent extends Token {
  override def toString: String = ")"
}

case class Word(value: String) extends Token {
  override def toString: String = value
}

object Comma extends Token {
  override def toString: String = ","
}

object Semi extends Token {
  override def toString: String = ";"
}

object End extends Token {
  override def toString: String = "$"
}

