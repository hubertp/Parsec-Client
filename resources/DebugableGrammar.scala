package com.github.hubertp.parserexperiments

import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.input._
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.debugging
import scala.util.parsing.combinator.debugging.Controllers
import debugging.ParserMacros._  

object DebugableGrammar extends DebugableTest {

  def main(args: Array[String]) {
    //val tokens = new lexical.Scanner(StreamReader(new java.io.InputStreamReader(System.in)))
    val tokens = new lexical.Scanner("Blip Blop Blap Blap Blap")
    //val tokens = new lexical.Scanner("succ succ succ of succ zero")
    //val tokens = new lexical.Scanner("succ succ succ zero")
    val mainParser = phrase(Term)
    mainParser(tokens) match {
      case Success(trees, _) =>
        try {
          println("Parsed as: " + trees)
        } catch {
          case tperror: Exception => println(tperror.toString)
        }
      case e =>

        println(e)
    }
  }
}

trait DebugableTest extends StandardTokenParsers with Controllers with debugging.DebugableParsers {

  def runMain(c : Controller) : Unit = {
    registerController(c)
    DebugableGrammar.main(Array(""))
  }

  lexical.delimiters ++= List("(", ")", "{", "}", ",", "*", "+")
  lexical.reserved   ++= List("true", "false", "succ",
                              "pred", "iszero", "zero", "of", "Mip", "Mup", "Map", "Blip", "Blop", "Blap", "Blup")
  
  def Term(implicit loc0: debugging.ParserLocation): Parser[Term] = (
    BoolTerm
     | "Blop" ~> "Blop" ~> "Blop" ^^^ True
     | "Blip" ~ ("Blup" | "Blop") ~ "Blap" ~ "Blap" ~ "Blap" ^^^ True
     | "Blip" ~ bopbop ~ "Blap" ~ "Blap" ~ "Blip" ^^^ True
  )

  def bopbop(implicit loc0: debugging.ParserLocation) : Parser[Term] = (
      "Mip" ^^^ True
    | "Mup" ^^^ True
    | "Map" ^^^ True
  )

//    | failure("illegal start of simple term"))
  
  def BoolTerm(implicit loc0: debugging.ParserLocation): Parser[Term] = (
    "true"   ^^^ True
    | "false"  ^^^ False
  )
  
}
