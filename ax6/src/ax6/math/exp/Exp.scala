
package ax6.math.exp

import ax6.math.ext._
import ax6.util.Text

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

abstract class Exp extends Ascii
  with Lambda    with MathML        with Latex
  with Calculate with Differentiate with Integrate with Precedence with Simplify
{
  type dbl = Double
  type LB  = ListBuffer[Exp]

  implicit def Int2Num( n:Int    ) : Exp = Num(n)
  implicit def dbl2Dbl( r:Double ) : Exp = Dbl(r)
  //implicit def str2Var( s:String ) : Exp = Var(s)

  // ... arithmetic operators from low to high precedence ....

  def equ ( v:Exp ) : Exp = Equ(this,v)
  def +   ( v:Exp ) : Exp = Add(this,v)
  def -   ( v:Exp ) : Exp = Sub(this,v)
  def *   ( v:Exp ) : Exp = Mul(this,v)
  def /   ( v:Exp ) : Exp = Div(this,v)
  def ~^  ( v:Exp ) : Exp = Pow(this,v) // ~^is used instead of ^ for highest precedence
  def unary_-       : Exp = Neg(this)

  def text:Text = { val t = Text(50); ascii(t,this); t }
  override def toString : String = text.toString
  def expAdd( u:Add ) : Exp = { u.asInstanceOf[Exp] }
  def expMul( u:Mul ) : Exp = { u.asInstanceOf[Exp] }
}

// Numbers and Variables
case class Num( n:Int )          extends Exp // n Int
case class Dbl( r:Double )       extends Exp // r Double 
case class Rat( n:Int, r:Int )   extends Exp // n / r
case class Var( s:String )       extends Exp // s String

// Binary Operators from lowest to highest precedence
case class Equ( u:Exp, v:Exp ) extends Exp   // u = v  Equation
case class Add( list:List[Exp]  ) extends Exp { // u + ...
  def map( func:Exp => Exp ) : Add =
  {
    val listBuf = new ListBuffer[Exp]()
    for( exp <- list ) listBuf += func( exp )
    Add(listBuf.toList)
  }
}
case class Sub( u:Exp, v:Exp ) extends Exp // u - v
case class Mul( list:List[Exp] ) extends Exp { // u * ...
  def map(func: Exp => Exp): Mul = {
    val listBuf = new ListBuffer[Exp]()
    for (exp <- list) listBuf += func(exp)
    Mul(listBuf.toList)
  }
}
case class Div( u:Exp, v:Exp ) extends Exp // u / v
case class Pow( u:Exp, v:Exp ) extends Exp // u ^ v

// Lists of Add and Muliply sequences
//case class Adds( exps:List[Exp] ) extends Exp
//case class Muls( exps:List[Exp] ) extends Exp

// Unary operator high precendence
case class Rec( u:Exp )          extends Exp // 1 / u
case class Neg( u:Exp )          extends Exp // -u

// Parenthesis Braces and Absolute Value
case class Par( u:Exp )        extends Exp // (u)
case class Brc( u:Exp )        extends Exp // {u}
case class Abs( u:Exp )        extends Exp // |u|

// Natural Log, Log Base, Root, Square Root and e
case class Lnn( u:Exp )         extends Exp // ln(u)
case class Log( u:Exp, r:Dbl )  extends Exp // log_b(u)
case class Roo( u:Exp, r:Dbl )  extends Exp // root_b(u)
case class Sqt( u:Exp )         extends Exp // sqrt(u)
case class Eee( u:Exp )         extends Exp // e^u

// Trigometric
case class Sin( u:Exp ) extends Exp
case class Cos( u:Exp ) extends Exp
case class Tan( u:Exp ) extends Exp
case class Sec( u:Exp ) extends Exp
case class Csc( u:Exp ) extends Exp
case class Cot( u:Exp ) extends Exp

// Inverse Trigometric
case class ASin( u:Exp ) extends Exp
case class ACos( u:Exp ) extends Exp
case class ATan( u:Exp ) extends Exp
case class ASec( u:Exp ) extends Exp
case class ACsc( u:Exp ) extends Exp
case class ACot( u:Exp ) extends Exp

// Differentiation, Limits, Integration, Summation and Integration with Lim
case class Dif( u:Exp )        extends Exp // d(u)   Differentiate
case class Lim( a:Exp, b:Exp ) extends Exp // _a^b limits for Sum and Int
case class Itg( u:Exp )        extends Exp // itg(u) Intergrate
case class Sum( a:Exp, b:Exp, u:Exp ) extends Exp
case class Itl( a:Exp, b:Exp, u:Exp ) extends Exp

// Subscript Superscipt Simplify Not are suspect
// Msg Err are also a minor concern
case class Sus( u:Exp, v:Exp ) extends Exp // x_1    Subscript   x1 -- place in Var
case class Sup( u:Exp, v:Exp ) extends Exp // x^1    Superscript x1 -- not used ^ => Pow
case class Not( u:Exp )        extends Exp // not an expression or can not be Intergrated
case class Sim( u:Exp )        extends Exp // sim(u) Simplify - just used for errors
case class Msg( m:Text )       extends Exp // error message from Parse

// Here we have Par remove uneeded parentheses
object Par {

  // Looks like a noop
  def apply( u:Exp ) : Exp = u match {
    case Add(a)   => Add(a)
    case Sub(a,b) => Sub(a,b)
    case a:Exp    => new Par(a)
  }
}

// Here we have Add take care of all its simplifications that can occur
//   with its binary apply constructors in AsciiParse
object Add {

//def apply( list:List[Exp] ) : Add = if( list.isEmpty ) Num(0) else new Add(list)

  def apply( list:List[Exp] ) : Add = new Add(list)
  
  def binOp( u:Exp, v:Exp ) : Add = {
    val list = new ListBuffer[Exp]()
    list += u
    list += v
    Add(list.toList) }

  def apply( u:Exp, v:Exp ) : Add = (u,v) match {
    case ( Add(a),    Add(b)  ) => Add(a,b)
    case ( Par(Add(a)), b:Exp ) => Add(a,b)
    case ( Add(a),      b:Exp ) => Add(a,b)
    case ( a:Exp, Par(Add(b)) ) => Add(a,b)
    case ( a:Exp,     Add(b)  ) => Add(a,b)
    case ( a:Exp,       b:Exp ) => Add.binOp(a,b) }

  def apply( u:List[Exp], v:Exp ) : Add = {
    val list = new ListBuffer[Exp]()
    for( e <- u ) list += e
    list += v
    Add(list.toList) }

  def apply( u:Exp, v:List[Exp] ) : Add = {
    val list = new ListBuffer[Exp]()
    list += u
    for( e <- v ) list += e
    Add(list.toList) }

  def apply( u:List[Exp], v:List[Exp] ) : Add = {
    val list = new ListBuffer[Exp]()
    for( a <- u ) list += a
    for( b <- v ) list += b
    Add(list.toList) }
}

// Here we have Mul take care of all its simplifications that can occur
//   with it binary apply conscructors in AsciiParse
object Mul {

  def apply( list:List[Exp] ) : Mul = new Mul(list)

  def binOp( u:Exp, v:Exp ) : Mul = {
    val list = new ListBuffer[Exp]()
    list += u
    list += v
    Mul(list.toList) }

  def apply( u:Exp, v:Exp ) : Mul = (u,v) match {
    case ( Mul(a),    Mul(b)  ) => Mul(a,b)
  //case ( Par(Mul(a)), b:Exp ) => Mul(a,b)
    case ( Mul(a),      b:Exp ) => Mul(a,b)
  //case ( a:Exp, Par(Mul(b)) ) => Mul(a,b)
    case ( a:Exp,     Mul(b)  ) => Mul(a,b)
    case ( a:Exp,       b:Exp ) => Mul.binOp(a,b) }

  def apply( u:List[Exp], v:Exp ) : Mul = {
    val list = new ListBuffer[Exp]()
    for( e <- u ) list += e
    list += v
    Mul(list.toList) }

  def apply( u:Exp, v:List[Exp] ) : Mul = {
    val list = new ListBuffer[Exp]()
    list += u
    for( e <- v ) list += e
    Mul(list.toList) }

  def apply( u:List[Exp], v:List[Exp] ) : Mul = {
    val list = new ListBuffer[Exp]()
    for( a <- u ) list += a
    for( b <- v ) list += b
    Mul(list.toList) }
}
