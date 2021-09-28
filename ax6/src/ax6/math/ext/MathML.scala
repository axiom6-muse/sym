
package ax6.math.ext

import  ax6.math.exp._
import  ax6.util.Text

trait MathML
{
  self:Exp =>
  
  def mathML( t:Text ): Unit = { mathML(t,this) }
  
  def mathML( t:Text, exp:Exp ): Unit = {
    exp match
    {
      case Num(n)    => mathML( t, "mn", n.toString )
      case Dbl(d)    => mathML( t, "mn", d.toString )
      case Rat(n,d)  => mathML( t, "mfrac", Num(n), Num(d) )
      case Var(s)    => mathML( t, "mi", s ) // Syms.entity(t,s)
      case Add(u,v)  => mathML( t, "mrow",   u, "+", v )
      case Sub(u,v)  => mathML( t, "mrow",   u, "-", v )
      case Mul(u,v)  => mathML( t, "mrow",   u, "*", v )
      case Div(u,v)  => mathML( t, "mfrac",  u, v )
      case Rec(u)    => mathML( t, "mfrac", Num(1), u )
      case Pow(u,v)  => mathML( t, "msup",   u, v )
      case Neg(u)    => mathML( t, "mo", "-" ); u.mathML(t)
      case Pls(u)    => mathML( t, "mo", "+" ); u.mathML(t)
      case Lis(exps) => listML( t, exps )
      case Abs(u)    => mathML( t, "mo", "|" ); u.mathML(t)
      case Par(u)    => mathML( t, "mfence", u )
      case Brc(u)    => mathML( t, "mfence", u )
      case Lnn(u)    => funcML( t, "ln",  u )
      case Log(u,b)  => mathML( t, "msub",  u, b )
      case Roo(u,r)  => mathML( t, "mroot", u, r )
      case Eee(u)    => mathML( t, "msup", Var("e"), u )
      case Sqt(u)    => mathML( t, "msqrt", u )
      case Sin(u)    => funcML( t, "sin",  u )
      case Cos(u)    => funcML( t, "cos",  u )
      case Tan(u)    => funcML( t, "tan",  u )
      case Csc(u)    => funcML( t, "csc",  u )
      case Sec(u)    => funcML( t, "sec",  u )
      case Cot(u)    => funcML( t, "cot",  u )
      case ASin(u)   => funcML( t, "arcsin", u )
      case ACos(u)   => funcML( t, "arccos", u )
      case ATan(u)   => funcML( t, "arctan", u )
      case ACsc(u)   => funcML( t, "arccsc", u )
      case ASec(u)   => funcML( t, "arcsec", u )
      case ACot(u)   => funcML( t, "arcot",  u )
      case Equ(u,v)  => mathML( t, "mrow",   u, "=", v )
      case Dif(u)    => mathML( t, "mi", "d" ); u.mathML(t)
      case Sus(u,v)  => mathML( t, "msub",    u, v )
      case Sup(u,v)  => mathML( t, "msup",    u, v )
      case Lim(u,v)  => mathML( t, "msupsup", u, v ) // Check
      case Itg(u)    => mathML( t, "mo", "&#x222B;" ); u.mathML(t)     // &Int;
      case Itl(a,b,u)=> operML( t, "msubsup",    "&#x222B;", a, b, u ) // &Int;
      case Sum(a,b,u)=> operML( t, "munderover", "&#x2211;", a, b, u ) // &Sum;
      case Cex(r,i)  => mathMLCex(t,r,i)
      case Vex(a)    => mathMLVex(t,a)
      case Mex(m)    => mathMLMex(t,m)
      case Msg(txt:Text) => t.app(txt)
    }
  }

  def mathML( t:Text, tag:String, s:String ): Unit =
    { t.all( '<', tag, '>', s, "</", tag, '>' ) }

  def mathML( t:Text, tag:String, u:Exp ): Unit =
    { t.all( '<', tag, '>' ); u.mathML(t); t.all( "</", tag, '>' ) }

  def funcML( t:Text, func:String, u:Exp ): Unit = {
    t.all( '<',  "mrow", '>' )
      mathML( t, "mi", func )
      mathML( t, "mfence", u )
    t.all( "</", "mrow", '>' )
  }

  def mathML( t:Text, tag:String, u:Exp, v:Exp ): Unit = {
    t.all( '<',  tag,  '>' )
      u.mathML(t)
      v.mathML(t)
    t.all( "</", tag, '>' )
  }

  def mathML( t:Text, tag:String, u:Exp, op:String, v:Exp ): Unit = {
    t.all( '<',  tag,  '>' )
      u.mathML(t)
      mathML( t, "mo", op )
      v.mathML(t)
    t.all( "</", tag, '>' )
  }
  
  def operML( t:Text, tag:String, op:String, a:Exp, b:Exp, u:Exp ): Unit = {
    t.all( '<',  tag,  '>' )
    mathML( t, "mo", op )
    a.mathML(t)
    b.mathML(t)
    t.all( "</", tag, '>' )
    u.mathML(t)
  }
  
  def mathMLCex( t:Text, r:Exp, i:Exp ): Unit =
    { r.mathML(t); i.mathML(t); t.app("<mi>i</mi>") }
    
  def mathMLVex( t:Text, a:Array[Exp] ): Unit = {
    t.app( "<mfenced open='[' close=']'>" )
    for( i <- a.indices )
      a(i).mathML(t)            // MathML takes care of commas
    t.app( "</mfenced>" )
  }

  def listML( t:Text, exps:List[Exp] ): Unit = {
    t.app( "<mfenced open='[' close=']'>" )
    for( exp <- exps )
      exp.mathML(t)            // MathML takes care of commas
    t.app( "</mfenced>" )
  }

  def mathMLMex( t:Text, mat:Array[Vex] ): Unit = {
    t.all( "<mfenced open='[' close=']'>", "<mtable>" )
    for( i <- mat.indices )
    { 
      t.app( "<mtr>" )
      for( j <- 0 until mat(i).n )
        { t.app("<mtd>"); mat(i)(j).mathML(t); t.app("</mtd>") }
      t.app( "</mtr>" )
    }
    t.all( "</mtable>", "</mfenced>" )
  }    
  
  def headML( t:Text ): Unit = {
    t.all( "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>", Text.eol )
    t.all( "<?xml-stylesheet type=\"text/css\" href=\"MathML.css\" ?>", Text.eol )
    t.all( "<root xmlns=\"http://www.w3.org/1998/Math/MathML\">", Text.eol )
  }

  def begML(  t:Text ): Unit = { t.app("<math>") }
  def endML(  t:Text ): Unit = { t.all("</math>", Text.eol ) }
  def footML( t:Text ): Unit = { t.all("</root>", Text.eol ) }


}