package ru.ifmo.rain.kokorin.parser

import ru.ifmo.rain.kokorin.lexer.Token

sealed class Tree(val description: String, val children: List[Tree]) {
  val symbol: String = this.getClass.getSimpleName

  private def toStringHelper(builder: StringBuilder){
    builder.append(s"[$description] of type [$symbol]\n")
    for {(child, number) <- children.zipWithIndex} {
      builder.append(s"Child number $number began\n")
      child.toStringHelper(builder)
      builder.append(s"Child number $number ended\n")
    }
  }

  override def toString: String = {
    val builder = StringBuilder.newBuilder
    toStringHelper(builder)
    builder.toString()
  }
}

case class S(override val children: List[Tree]) extends Tree("Function declaration", children)

case class T(override val children: List[Tree]) extends Tree(s"Base part of type", children)

case class TPrime(empty: Boolean, override val children: List[Tree])
  extends Tree(if (empty) "Empty tail of *-part of type" else "Tail of *-part of type", children)

case class A(empty: Boolean, override val children: List[Tree])
  extends Tree(if (empty) "Empty arguments list" else "Arguments list", children)

case class APrime(empty: Boolean, override val children: List[Tree])
  extends Tree(if (empty) "Empty tail of arguments list" else "Tail of arguments list", children)

case class B(override val children: List[Tree]) extends Tree("Single argument", children)

case class NTerm(token: Token) extends Tree(token.toString, Nil)
