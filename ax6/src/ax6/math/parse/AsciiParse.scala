
package ax6.math.parse

import ax6.util.Text
import ax6.math.exp._

import scala.collection.mutable.ListBuffer
//import  scala.util.parsing.util.parsing.input
//import  scala.util.parsing.combinator.Parsers
import  scala.util.parsing.combinator.lexical.StdLexical
import  scala.util.parsing.combinator.syntactical.StdTokenParsers

// Need to consider match not exhaustive warning

sealed abstract class Parser[Exp]

object AsciiParse extends StdTokenParsers 
{
  type Tokens = StdLexical
  
  val lexical = new StdLexical
  
  lexical.delimiters ++= 
    List( "(",")","[","]","{","}","|",",","+","-","^","/","*","_","=","." )

  lexical.reserved   ++=
    List("d", "e", "i", "root","sum","Int",
    "sqrt","ln","log","sin","cos","tan","cot","sec","csc",  
    "arcsin","arccos","arctan","arccot","arcsec","arccsc")

 
  // Need to lex Double
  def NUM : Parser[Num] = numericLit ^^ { (s:String) => Num(s.toInt)    }
  def DBL : Parser[Dbl] = numericLit ^^ { (s:String) => Dbl(s.toDouble) }
  def VAR : Parser[Exp] = ident      ^^ { (s:String) => variable(s)     }
  
  def variable( s:String ) : Exp = 
  {
    if( s.charAt(0)=='d' && s.length==2 )  // Special case where Dif d
      Dif(Var(s.charAt(1).toString))       // is split from a variable
    else
      Var(s)
  }

  def toAdd( u:Exp, v:Exp ) : Exp = { (u,v) match {
    case ( Par(Add(a)), _ ) => Add(a)
    case ( Add(a), b: Exp ) => toList("Add",a,b)
    case ( a: Exp, b: Exp ) => toMake("Add",a,b)
  } }

  def toMul( u:Exp, v:Exp ) : Exp = { (u,v) match {
    case ( Mul(a), b: Exp ) => toList("Mul",a,b)
    case ( a: Exp, b: Exp ) => toMake("Mul",a,b)
  } }

  def toList( adt:String, a:List[Exp], b:Exp ) : Exp = {
    val list = new ListBuffer[Exp]()
    for( e <- a ) list += e
    list += b
    if( adt=="Add" ) Add(list.toList) else Mul(list.toList)
  }

  def toMake( adt:String, a:Exp, b:Exp ) : Exp = {
    val list = new ListBuffer[Exp]()
    list += a
    list += b
    if( adt=="Add" ) Add(list.toList) else Mul(list.toList)
  }

  // ... Binary pass through parsers from high to low precedence
  def beg : Parser[Exp] = base | oper
  def pow : Parser[Exp] = beg * ( "^" ^^^ { (u:Exp,v:Exp) => Pow(u,v)   } )  // u ^ v   Pow(u,v)
  def mul : Parser[Exp] = pow * ( "*" ^^^ { (u:Exp,v:Exp) => toMul(u,v) } )  // u * ... Mul(u...)
  def div : Parser[Exp] = mul * ( "/" ^^^ { (u:Exp,v:Exp) => Div(u,v)   } )  // u / v   Div(u,v)
  def add : Parser[Exp] = div * ( "+" ^^^ { (u:Exp,v:Exp) => toAdd(u,v) } )  // u + ... Add(u...)
  def sub : Parser[Exp] = add * ( "-" ^^^ { (u:Exp,v:Exp) => Sub(u,v)   } )  // u - v   Sub(u,v)
  def equ : Parser[Exp] = sub * ( "=" ^^^ { (u:Exp,v:Exp) => Equ(u,v)   } )  // u = v   Equ(u,v)
  def end : Parser[Exp] = equ

  //  def rem : Parser[Exp] = repsep(beg,"*") ^^ { (u:List[Exp]) => Muls(u) }
  //  def rea : Parser[Exp] = repsep(beg,"+") ^^ { (u:List[Exp]) => Adds(u) }
  //def rem : Parser[List[Exp]] = repsep(beg,"*")
  //def rea : Parser[List[Exp]] = repsep(beg,"+")
  //def muls : Parser[Exp] = "(" ~> rem <~ ")" ^^ { (u:List[Exp]) => Muls(u) }
  //def adds : Parser[Exp] = "(" ~> rea <~ ")" ^^ { (u:List[Exp]) => Adds(u) }
  //def add  : Parser[Exp] = div * ( "+" ^^^ { (u:Exp,v:Exp) => Add(u,v) } )  // u + v  Add(u,v)
  //def mul  : Parser[Exp] = rem        ^^  { (u:List[Exp]) => Muls(u) }
  //def con  : Parser[Exp] = { (pow ~ mul) ^^ { case u ~ v => Add(u,v) } }
  //def seq  : Parser[Exp] = { (pow | mul) ^^ { case u * v => Add(u,v) } }
  
  // ... Grouping ...
                    
  def par : Parser[Exp] = "(" ~> end <~ ")"  ^^ { (u:Exp) => Par(u) }    
  def brc : Parser[Exp] = "{" ~> end <~ "}"  ^^ { (u:Exp) => Brc(u) }    
  def abs : Parser[Exp] = "|" ~> end <~ "|"  ^^ { (u:Exp) => Abs(u) }    
  
  // ... cex vex mex ...
  def iii : Parser[Exp] = "i" ^^ { case "i" => Var("i") }
  def cex : Parser[Exp] = "[" ~> end ~ "," ~ end <~ "." <~ "i"  <~ "]"  ^^
   { case u ~ "," ~ v => new Cex(u,v) }

  def vel : Parser[List[Exp]] = repsep(end, ",")
  def ves : Parser[List[Exp]] = rep(vex)
  def vex : Parser[Exp] = "[" ~> vel <~ "]" ^^ { (u:List[Exp]) => new Vex(u) }
  def mex : Parser[Exp] = "[" ~> ves <~ "]" ^^ { (u:List[Exp]) => new Mex(u) }
  
  // ... func(arg) ln, logb root ...

  def arg : Parser[Exp] = "(" ~> end <~ ")" // consumes (u) for functions
  
  def fun : Parser[Exp] = farg ~ arg ^^  { case f ~ u  => func(f,u) }

  //def fer : Parser[Exp] = stringLit ~ arg ^^  { case f ~ u  => func(f,u) }
  
  def farg : Parser[String] =  // Functions with a single argument
    "sqrt"|"ln"|"log"|"sin"|"cos"|"tan"|"cot"|"sec"|"csc"|
    "arcsin"|"arccos"|"arctan"|"arccot"|"arcsec"|"arccsc"|"d" 

  // Check case where "log" => Lnn(u)
  def func( f:String, u:Exp ) : Exp = f match {
    case "sqrt"   => Sqt(u)
    case "ln"     => Lnn(u)
    case "log"    => Lnn(u)
    case "sin"    => Sin(u)
    case "cos"    => Cos(u)
    case "tan"    => Tan(u)
    case "cot"    => Cot(u)
    case "sec"    => Sec(u)
    case "csc"    => Csc(u)
    case "arcsin" => ASin(u)
    case "arccos" => ACos(u)
    case "arctan" => ATan(u)
    case "arccot" => ACot(u)
    case "arcsec" => ASec(u)
    case "arccsc" => ACsc(u)
    case "d"      => Dif(u)
    case _ => Msg(Text(50).text("Ascii.func::", f, '(', u.text, ')', " :: is an unknown function"))
  }
  
  // log Interpreted as base e same as Ln for other bases use log_b(arg)
  // e^u should not be a problem since "e" is reserved
  // ^ not a problem for Int and sum since lim consumes _ a ^ b
  // def eee : Parser[Exp] = ("e"|Var("e")) ~> "^" ~ end ^^
  def log : Parser[Exp] = "log"  ~> "_" ~> DBL ~ arg  ^^ { case  b  ~  u => Log(u,b) }
  def roo : Parser[Exp] = "root" ~> "_" ~> DBL ~ arg  ^^ { case  r  ~  u => Roo(u,r) }
  def lim : Parser[Exp] = "_" ~ beg ~ "^"  ~ beg      ^^ { case "_" ~  a ~ "^" ~ b => Lim(a,b) }

  def sum  : Parser[Exp] = ("Int"|"sum") ~ lim ~ arg  ^^
  {
      case "sum" ~ Lim(a,b) ~ u => Sum(a,b,u)
      case "Int" ~ Lim(a,b) ~ u => Itl(a,b,u)
  }
     
  def neg : Parser[Exp] = "-" ~ end ^^ { case "-" ~ u => Neg(u) }

  // e is a reserved exponentialion keyword that must be followed by ^
  // e^u is handled by ee1
  // e standalone variable is handle by ee2
  def ee1 : Parser[Exp] =  "e" ~ "^" ~ end  ^^ { case "e" ~ "^" ~ u => Eee(u)   }
  def ee2 : Parser[Exp] =  "e"              ^^ { case "e"           => Var("e") }
  def eee : Parser[Exp] =  ee1 | ee2
 
  // d is a reserved differentiation keyword.
  // dx is handled by variable() converts dx Into Into Dif(Var(x)) fo just a 1 Char variable
  //   since we have to split a variable string the call by VAR to variable() is needed
  // d(exp) or differentiation of an expression is handled by dif1
  // d standalone variable is handled by dif2
  def dif1 : Parser[Exp] = "d" ~ end ^^ { case "d" ~ u => Dif(u)   }  // ??? need noparen
  def dif2 : Parser[Exp] = "d"       ^^ { case "d"     => Var("d") }
  def dif  : Parser[Exp] =  dif1 | dif2

  // Right now subscript and superscript are a problem with stack overflows
  // def sus : Parser[Exp] = VAR ~ "_" ~ NUM ^^ { case u ~ "_" ~ v => Sus(u,v) }
  // def sup : Parser[Exp] = end ~ "^" ~ end ^^ { case u ~ "^" ~ v => Sup(u,v) }
 
  // ...... Primary (pri) and Production (exp) Parser Rules ......
  
  def base:Parser[Exp] = NUM | dif | VAR | par | brc | abs | cex | vex | mex

  def oper:Parser[Exp] = fun | log | roo | eee | iii | sum | neg  // | sus | sup

  // Everything is an expression (expr) or an equation (equ)
  def all:Parser[Exp] = end | failure("end")
   
  def apply( str:String ) : Exp = parse( str )
                 
  def parse( str:String ) : Exp = all( new lexical.Scanner(str) ) match
  {
  //case Success( Var(s), _ ) => Msg( Text(50).text( "Ascii.parse Warning::<", str, ">::", "entire expression parsed Into a single variable" ) )
    case Success( exp, _  ) => exp
    case Failure( msg, _  ) => Msg( Text(50).text( "Ascii.parse Failure::<", str, ">::", msg  ) )
    case Error(   msg, _  ) => Msg( Text(50).text( "Ascii.parse Error::<",   str, ">::", msg  ) )
  }

}