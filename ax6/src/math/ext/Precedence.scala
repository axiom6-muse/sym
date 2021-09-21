package ax6.math.ext

import  ax6.math.exp._
import  ax6.util.{ Log=>Logg }

trait Precedence
{
  self:Exp =>
  
  def precedence( op:String ) : Int = op match
  {

    case "="       =>    0
    case "||"      =>    1  // "|" is use for absolute value
    case "&" |"&&" =>    2
    case "=="|"!=" =>    3
    case "<" |"<="|">"|">=" => 4
    case "+" |"-" =>     5
    case "*" |"%" =>     6    
    case "/"      =>     7
    case "^"      =>     8
    case "_"      =>     9
    case "sum"|"itg" => 10
    case "unary"|"unary-"|"unary+"   => 11
    case "d"|"dif"                   => 12
    case "func"                      => 13
    case "cex"|"vex"|"mex"           => 14 // ???
    case "num"|"dbl"|"rat"|"var"     => 15 // ???
    case "("|")"|"{"|"}"|"["|"]"|"|" => 16 // paranthesies very high                 
    case _ => Logg.error( "Unknown", op, "for precedence" ); highest
  }    
  val highest = 15
    
  val equ:Int = precedence("=")
  val or:Int    = precedence("||")
  val and:Int   = precedence("&&")
  val equalto:Int=precedence("==")
  val lt:Int    = precedence("<")
  val le:Int    = precedence("<=")
  val gt:Int    = precedence(">")
  val ge:Int    = precedence("<=")
  val add:Int   = precedence("+")
  val sub:Int   = precedence("-")
  val mul:Int   = precedence("*")
  val div:Int   = precedence("/")  // -1 for now
  val sus:Int  = precedence("_") // Subscript
  val sup:Int   = precedence("_")
  val pow:Int   = precedence("^")
  val sum:Int   = precedence("sum")
  val neg:Int   = precedence("unary-")
  val differ:Int = precedence("dif")
  val func:Int  = precedence("func")
  val vex:Int   = precedence("vex")
  val num:Int   = precedence("num")
  val par:Int = precedence("(")
  val abs:Int   = precedence("|")
  
  def precedence( exp:Exp ) : Int = exp match
  {
    case Num(_)    => num
    case Dbl(_)    => num
    case Rat(_,_)  => num
    case Var(_)    => num
    case Add(_,_)  => add
    case Sub(_,_)  => sub
    case Mul(_,_)  => mul
    case Div(_,_)  => div
    case Rec(_)    => div
    case Pow(_,_)  => pow
    case Neg(_)    => neg
    case Abs(_)    => abs
    case Par(_)    => par
    case Brc(_)    => par
    case Lnn(_)    => func
    case Log(_,_)  => func
    case Roo(_,_)  => func
    case Eee(_)    => func
    case Sqt(_)    => func
    case Sin(_)    => func
    case Cos(_)    => func
    case Tan(_)    => func
    case Csc(_)    => func
    case Sec(_)    => func
    case Cot(_)    => func
    case ASin(_)   => func
    case ACos(_)   => func
    case ATan(_)   => func
    case ACsc(_)   => func
    case ASec(_)   => func
    case ACot(_)   => func
    case Equ(_,_)  => equ
    case Dif(_)    => differ
    case Sus(_,_)  => func
    case Sup(_,_)  => func
    case Lim(_,_)  => func
    case Itg(_)    => sum
    case Itl(_,_,_)=> sum
    case Sum(_,_,_)=> sum
    case Cex(_,_)  => vex
    case Vex(_)    => vex
    case Mex(_)    => vex
    case Not(_)    => func
    case Msg(_)    => func
  }
 
// ADT operators with spacing for the lower precendence
   def operator( exp:Exp ) : String = exp match
   {
     case Equ(_,_)  => " = "
     case Add(_,_)  => "+"
     case Sub(_,_)  => "-"
     case Mul(_,_)  => "*"
     case Div(_,_)  => "/"
     case Pow(_,_)  => "^"
     case Eee(_)    => "^"
     case Neg(_)    => "unary-"
     case Dif(_)    => "d"
     case Itg(_)    => "Int"
     case Itl(_,_,_)=> "Int"
     case Sum(_,_,_)=> "sum"
     case _         => ""
  }
  
}