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
        NTerm(Word("int")) :: TPrime(empty = true, Nil) :: Nil
      ) :: NTerm(Word("f"))
        :: NTerm(LeftParent)
        :: A(empty = true, Nil)
        :: NTerm(RightParent)
        :: NTerm(Semi)
        :: Nil
    )
    assert(tree == expectedResult)
  }

  it should "parse function declaration with pointer-types" in {
    val parsedString = new StringReader("int** f();")
    val tree = Parser.parse(parsedString)
    val expectedResult = S(
      T(
        NTerm(Word("int"))
          :: TPrime(
          empty = false,
          NTerm(Asterisk) :: TPrime (
            empty = false,
            NTerm(Asterisk) :: TPrime(empty = true, Nil) :: Nil
          ) :: Nil
        ) :: Nil
      ) :: NTerm(Word("f"))
        :: NTerm(LeftParent)
        :: A(empty = true, Nil)
        :: NTerm(RightParent)
        :: NTerm(Semi)
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
      empty = false,
      B(
        T(
          NTerm(Word("int")) :: TPrime(empty = true, Nil) :: Nil
        ) :: NTerm(Word("a")) :: Nil
      ) :: APrime(empty = true, Nil) :: Nil
    )
    val expectedResult = S(
      T(
        NTerm(Word("int")) :: TPrime(empty = true, Nil) :: Nil
      ) :: NTerm(Word("f"))
        :: NTerm(LeftParent)
        :: argsList
        :: NTerm(RightParent)
        :: NTerm(Semi)
        :: Nil
    )
    assert(tree == expectedResult)
  }

  it should "Parse string with 2 and more arguments" in {
    val parsedString = new StringReader("int f(int a, float b);")
    val tree = Parser.parse(parsedString)

    val argsListTail = APrime(
      empty = false,
      NTerm(Comma) :: A(
        empty = false,
        B(
          T(
            NTerm(Word("float")) :: TPrime(empty = true, Nil) :: Nil
          ) :: NTerm(Word("b")) :: Nil
        ) :: APrime(empty = true, Nil) :: Nil
      ) :: Nil
    )

    val argsList = A(
      empty = false,
      B(
        T(
          NTerm(Word("int")) :: TPrime(empty = true, Nil) :: Nil
        ) :: NTerm(Word("a")) :: Nil
      ) :: argsListTail :: Nil
    )
    val expectedResult = S(
      T(
        NTerm(Word("int")) :: TPrime(empty = true, Nil) :: Nil
      ) :: NTerm(Word("f"))
        :: NTerm(LeftParent)
        :: argsList
        :: NTerm(RightParent)
        :: NTerm(Semi)
        :: Nil
    )

    assert(tree == expectedResult)
  }

  it should "not consider incorrect trees correct" in {
    val parsedString = new StringReader("int f(int a, float b, double c);")
    val tree = Parser.parse(parsedString)

    val argsListTail = APrime(
      empty = false,
      NTerm(Comma) :: A(
        empty = false,
        B(
          T(
            NTerm(Word("float")) :: TPrime(empty = true, Nil) :: Nil
          ) :: NTerm(Word("b")) :: Nil
        ) :: APrime(empty = true, Nil) :: Nil
      ) :: Nil
    )

    val argsList = A(
      empty = false,
      B(
        T(
          NTerm(Word("int")) :: TPrime(empty = true, Nil) :: Nil
        ) :: NTerm(Word("a")) :: Nil
      ) :: argsListTail :: Nil
    )
    val expectedResult = S(
      T(
        NTerm(Word("int")) :: TPrime(empty = true, Nil) :: Nil
      ) :: NTerm(Word("f"))
        :: NTerm(LeftParent)
        :: argsList
        :: NTerm(RightParent)
        :: NTerm(Semi)
        :: Nil
    )

    assert(tree != expectedResult)
  }
}
