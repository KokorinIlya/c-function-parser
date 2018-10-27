package ru.ifmo.rain.kokorin.parser

import ru.ifmo.rain.kokorin.lexer.Token

import scala.annotation.tailrec

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

object Tree {
  private[parser] def compareChildren(aChildren: List[Tree], bChildren: List[Tree]): Boolean = {
    (aChildren, bChildren) match {
      case (Nil, Nil) => true

      case (aFirst :: aTail, bFirst :: bTail) =>
        val first = treesEqual(aFirst, bFirst)
        val second = compareChildren(aTail, bTail)
        first && second

      case _ => false
    }
  }

  def treesEqual(a: Tree, b: Tree): Boolean = {
    (a, b) match {
      case (S(thisChildren), S(objChildred)) =>
        val res = compareChildren(thisChildren, objChildred)
        res

      case (T(thisChildren), T(objChildred)) =>
        val res = compareChildren(thisChildren, objChildred)
        res

      case (TPrime(thisEmpty, thisChildren), TPrime(otherEmpty, objChildred)) =>
        val first = thisEmpty == otherEmpty
        val second = compareChildren(thisChildren, objChildred)
        first && second


      case (A(thisEmpty, thisChildren), A(otherEmpty, objChildred)) =>
        val first = thisEmpty == otherEmpty
        val second = compareChildren(thisChildren, objChildred)
        first && second


      case (APrime(thisEmpty, thisChildren), APrime(otherEmpty, objChildred)) =>
        val first = thisEmpty == otherEmpty
        val second = compareChildren(thisChildren, objChildred)
         first && second

      case (B(thisChildren), B(objChildred)) =>
        val res = compareChildren(thisChildren, objChildred)
        res

      case (NTerm(thisToken), NTerm(otherToken)) =>
        val res = Token.tokensEqual(thisToken, otherToken)
        res

      case _ => false
    }
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
