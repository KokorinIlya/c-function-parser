package ru.ifmo.rain.kokorin.parser

import java.io.StringReader

import org.scalatest.FlatSpec
import ru.ifmo.rain.kokorin.lexer._

class ParserTest extends FlatSpec {
  "Parser" should "parse trivial function declarations" in {
    val parsedString = new StringReader("int f();")
    val tree = Parser.parse(parsedString)
    val expectedResult = S(
      T(
        TokenHolder(Word("int")) :: TPrime() :: Nil
      ) :: TokenHolder(Word("f"))
        :: TokenHolder(LeftParent)
        :: A()
        :: TokenHolder(RightParent)
        :: TokenHolder(Semi)
        :: Nil
    )
    assert(tree == expectedResult)
  }

  it should "parse function declaration with pointer-types" in {
    val parsedString = new StringReader("int** f();")
    val tree = Parser.parse(parsedString)
    val expectedResult = S(
      T(
        TokenHolder(Word("int"))
          :: TPrime(
          TokenHolder(Asterisk) :: TPrime (
            TokenHolder(Asterisk) :: TPrime() :: Nil
          ) :: Nil
        ) :: Nil
      ) :: TokenHolder(Word("f"))
        :: TokenHolder(LeftParent)
        :: A()
        :: TokenHolder(RightParent)
        :: TokenHolder(Semi)
        :: Nil
    )
    assert(tree == expectedResult)
  }

  it should "throw exception if incorrect declaration is given" in {
    val parsedString = new StringReader("void f(int* ;);")
    assertThrows[ParseException](Parser.parse(parsedString))
  }

  it should "parse functions with single argument" in {
    val parsedString = new StringReader("int f(int a);")
    val tree = Parser.parse(parsedString)
    val argsList = A(
      B(
        T(
          TokenHolder(Word("int")) :: TPrime() :: Nil
        ) :: TokenHolder(Word("a")) :: Nil
      ) :: APrime() :: Nil
    )
    val expectedResult = S(
      T(
        TokenHolder(Word("int")) :: TPrime() :: Nil
      ) :: TokenHolder(Word("f"))
        :: TokenHolder(LeftParent)
        :: argsList
        :: TokenHolder(RightParent)
        :: TokenHolder(Semi)
        :: Nil
    )
    assert(tree == expectedResult)
  }

  it should "Parse string with 2 and more arguments" in {
    val parsedString = new StringReader("int f(int a, float b);")
    val tree = Parser.parse(parsedString)

    val argsListTail = APrime(
      TokenHolder(Comma) :: A(
        B(
          T(
            TokenHolder(Word("float")) :: TPrime() :: Nil
          ) :: TokenHolder(Word("b")) :: Nil
        ) :: APrime() :: Nil
      ) :: Nil
    )

    val argsList = A(
      B(
        T(
          TokenHolder(Word("int")) :: TPrime() :: Nil
        ) :: TokenHolder(Word("a")) :: Nil
      ) :: argsListTail :: Nil
    )
    val expectedResult = S(
      T(
        TokenHolder(Word("int")) :: TPrime() :: Nil
      ) :: TokenHolder(Word("f"))
        :: TokenHolder(LeftParent)
        :: argsList
        :: TokenHolder(RightParent)
        :: TokenHolder(Semi)
        :: Nil
    )

    assert(tree == expectedResult)
  }

  it should "not consider incorrect trees correct" in {
    val parsedString = new StringReader("int f(int a, float b, double c);")
    val tree = Parser.parse(parsedString)

    val argsListTail = APrime(
      TokenHolder(Comma) :: A(
        B(
          T(
            TokenHolder(Word("float")) :: TPrime() :: Nil
          ) :: TokenHolder(Word("b")) :: Nil
        ) :: APrime() :: Nil
      ) :: Nil
    )

    val argsList = A(
      B(
        T(
          TokenHolder(Word("int")) :: TPrime() :: Nil
        ) :: TokenHolder(Word("a")) :: Nil
      ) :: argsListTail :: Nil
    )
    val expectedResult = S(
      T(
        TokenHolder(Word("int")) :: TPrime() :: Nil
      ) :: TokenHolder(Word("f"))
        :: TokenHolder(LeftParent)
        :: argsList
        :: TokenHolder(RightParent)
        :: TokenHolder(Semi)
        :: Nil
    )

    assert(tree != expectedResult)
  }

  it should "make correct string representation" in {
    val string = "double*** f(int** a, float** b, double***** c);"
    val parsedString = new StringReader(string)
    val tree = Parser.parse(parsedString)
    assert(tree.toString == string)
  }
}
