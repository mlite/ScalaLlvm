package org.scalair.ir.llvm

/**
 * User: wangn
 * Date: Mar 8, 2011
 */
import util.parsing.combinator.lexical.StdLexical
import util.parsing.combinator.token.StdTokens
import util.parsing.input.CharArrayReader


trait LLVMTokens extends StdTokens {
  case class VoidTok(chars: String) extends Token { override def toString = "VoidTok " + chars }
  case class TypeTok(chars: String) extends Token { override def toString = "TypeTok " + chars }
  case class CConvTok(chars: String) extends Token { override def toString = "CconvTok " + chars }
  case class GlobalVarTok(chars:String) extends Token { override def toString = "GlobalVarTok " + chars }
  case class GlobalIDTok(chars:String) extends Token { override def toString = "GlobalIdTok " + chars }
  case class LocalVarTok(chars:String) extends Token { override def toString = "LocalVarTok " + chars }
  case class LocalIDTok(chars:String) extends Token { override def toString = "LocalIdTok " + chars }

  case class MDVarTok(chars:String) extends Token { override def toString = "MDVarTok " + chars }
  case class DotDotDotTok(chars:String) extends Token { override def toString = "..." }

  case class LabelStrTok(chars:String) extends Token { override def toString = "LabelStrTok " + chars }
  case class NIntTok(chars:String) extends Token { override def toString = "NIntTok " + chars }
  case class PIntTok(chars:String) extends Token { override def toString = "PIntTok " + chars }
  case class FPTok(chars:String) extends Token { override def toString = "FPTok " + chars }
  case class HexIntTok(chars:String) extends Token { override def toString = "HexIntTok " + chars }
  case class HexFPTok(chars:String) extends Token { override def toString = "HexFpTok " + chars }
  case class HexFP80Tok(chars:String) extends Token { override def toString = "HexFp80Tok " + chars }
  case class HexFP128Tok(chars:String) extends Token { override def toString = "HexFp128Tok " + chars }
  case class HexPPC128Tok(chars:String) extends Token { override def toString = "HexFp128Tok " + chars }
}

object LLVMLexer extends StdLexical with LLVMTokens {
  def lexIdentifier =
    (rep(labelChar) <~ ':' ^^ { l => LabelStrTok(l.mkString("")) }
      | 'i' ~> rep1(digit) ^^ { l => TypeTok("i" + l.mkString("")) }
      | 'c' ~> 'c' ~> rep1(digit) ^^ { l => CConvTok(l.mkString("")) }
      | 'u' ~ '0' ~ 'x' ~ rep1(hexChar) ^^ { case 'u' ~ '0' ~'x' ~ l => HexIntTok(l mkString "") }
      | 's' ~ '0' ~ 'x' ~ rep1(hexChar) ^^ { case 's' ~ '0' ~'x' ~ l => HexIntTok(l mkString "") }
      | identChar ~ rep( identChar | digit ) ^^ { case first ~ rest => processIdent(first :: rest mkString "") })

  def quoteString =
    ('\"' ~ rep( chrExcept('\"', CharArrayReader.EofCh) ) ~ '\"' ^^
      { case '\"' ~ chars ~ '\"' => chars mkString "" }
      | '\"' ~> failure("unclosed string literal")
      )

  def digitString = rep1(digit) ^^ { case l => l mkString "" }
  def nonDigitIdentChar = letter | elem ('$') | elem ('.') | elem ('_') | elem ('-')
  def otherIdentChar = letter | digit | elem ('$') | elem ('.') | elem ('_') | elem ('-')
  def labelChar = otherIdentChar

  def hexChar = (digit | elem('a') | elem('b') | elem ('c') | elem('d') | elem('e') | elem('f')
    | elem('A') | elem('B') | elem ('C') | elem('D') | elem('E') | elem('F'))

  def identString = nonDigitIdentChar ~ rep(otherIdentChar) ^^ { case first ~ rest => first::rest mkString "" }

  // -[0-9]+
  def nIntConst = '-' ~ rep1(digit) ^^ { case n ~ l => NIntTok ("-" +l.mkString("")) }

  //  [0-9]+
  def pIntConst = rep1(digit) ^^ { case l => PIntTok(l mkString "") }

  //  [-+]?[0-9]+[.][0-9]*([eE][-+]?[0-9]+)?
  def fpConst = {
    def signStr = elem('-') | elem ('+')
    def expStr = (elem('e') | elem ('E')) ~ opt (signStr) ~ rep1 (digit) ^^
      { case e ~ sign ~ ds =>
        e.toString + (sign match
        {
          case Some(s) => s
          case None => ""
        }) + (ds mkString "")
      }

    opt(signStr) ~ rep1(digit) ~ elem ('.') ~ rep(digit) ~ opt (expStr) ^^
      { case signOpt ~ d ~ '.' ~ p ~ expOpt =>
        val s0 = signOpt match { case Some(s) => s case None => "" }
        val s1 = expOpt match { case Some(s) => s case None => "" }
        FPTok(s0 + (d mkString "") + "." + (p mkString "") + s1)
      }
  }

  def hexFPConst =
    ('0' ~ 'x' ~ rep1(hexChar) ^^ { case '0' ~ 'x' ~ cs => HexFPTok("0x" + cs.mkString("")) }
      | '0' ~ 'x' ~ 'K' ~ rep1(hexChar) ^^ { case '0' ~ 'x' ~ 'K' ~ l => HexFP80Tok ("0xK" + l.mkString("")) }
      | '0' ~ 'x' ~ 'L' ~ rep1(hexChar) ^^ { case '0' ~ 'x' ~ 'L' ~ l => HexFP128Tok("0xL" + l.mkString("")) }
      | '0' ~ 'x' ~ 'M' ~ rep1(hexChar) ^^ { case '0' ~ 'x' ~ 'M' ~ l => HexFP128Tok("0xM" + l.mkString("")) })

  def dotDotDotToken = repN(3, elem('.')) ^^ { case _ => DotDotDotTok("...") }

  def lexAt =
    (elem('@') ~ digitString ^^ { case at ~ id => GlobalIDTok(id) }
      | elem('@') ~ quoteString ^^ { case at ~ str => GlobalVarTok("\"" + str + "\"") }
      | elem('@') ~ identString ^^ { case at ~ str => GlobalVarTok(str) })

  def lexPercent =
    (elem('%') ~ digitString ^^ { case _ ~ id => LocalIDTok(id) }
      | elem('%') ~ quoteString ^^ { case _ ~ str => LocalVarTok("\"" + str + "\"") }
      | elem('%') ~ identString ^^ { case _ ~ str => LocalVarTok(str) })

  def lexQuote =
    quoteString ~ opt(':') ^^
      {
        case str ~ Some(_) => LabelStrTok("\"" + str + "\"")
        case s ~ None => StringLit(s)
      }

  def lexExclam =
    elem('!') ~ letter ~ rep(otherIdentChar) ^^ { case c ~ first ~ rest => MDVarTok (first::rest mkString "") }

  def lexDigitOrNegative = (hexFPConst | fpConst | nIntConst | pIntConst)

  override def processIdent (s:String) =
    s match {
      case "void" => VoidTok("void")
      case "float" => TypeTok("float")
      case "double" => TypeTok("double")
      case "fp128" => TypeTok("fp128")
      case "x86_fp80" => TypeTok("x86_fp80")
      case "ppc_fp128" => TypeTok("ppc_fp128")
      case "x86mmx" => TypeTok("x86mmx")
      case "label" => TypeTok("label")
      case "metadata" => TypeTok("metadata")
      case "opaque" => TypeTok("opaque")
      case "f32" => TypeTok("f32")
      case "f64" => TypeTok("f64")
      case "f80" => TypeTok("f80")
      case "f128" => TypeTok("f128")
      case "v64" => TypeTok("v64")
      case "v128" => TypeTok("v128")
      case _ => {
        if (reserved contains s) Keyword(s)
        else Identifier(s)
      }
    }

  override def token:Parser[Token] =
    (lexIdentifier
      | elem('+') ~> fpConst
      | lexAt
      | lexPercent
      | lexQuote
      | dotDotDotToken
      | lexExclam
      | lexDigitOrNegative
      | CharArrayReader.EofCh  ^^^ EOF
      | delim
      | failure("illegal character")
      )

  override def whitespace: Parser[Any] = rep(whitespaceChar|comment)

  override def comment: Parser[String] =
    elem(';') ~ rep(chrExcept(CharArrayReader.EofCh,'\n')) ~ '\n' ^^
      { case ';' ~ l ~ '\n' => ";" + (l mkString "") + "\n" }
}