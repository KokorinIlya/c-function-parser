package ru.ifmo.rain.kokorin.lexer

import ru.ifmo.rain.kokorin.parser.NTerm

sealed trait Token {
  def toTree: NTerm = NTerm(this)
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

