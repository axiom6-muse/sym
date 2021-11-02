
package ax6.math.ext

import  ax6.math.exp._
import  ax6.util.Text

trait Ast
{
  self:Exp =>

  def toAst : String = {
    val t:Text = new Text(100)
    ast( t )
    t.toStr
  }
    
  def ast( t:Text ): Unit = { ast(t,this) }
  
  def ast( t:Text, exp:Exp ): Unit = {
    exp match
    {
      case Num(n)    => astStr2( t, "Num", n.toString )
      case Dbl(r)    => astStr2( t, "Dbl", r.toString )
      case Rat(n,d)  => astStr3( t, "Rat", n.toString, d.toString )
      case Var(s)    => astStr2( t, "Var", s )
      case Add(u,v)  => astBins( t, "Add", u, v )
      case Sub(u,v)  => astBins( t, "Sub", u, v )
      case Mul(u,v)  => astBins( t, "Mul", u, v )
      case Div(u,v)  => astBins( t, "Div", u, v )
      case Rec(u)    => astRec1( t, "Rec", u )
      case Pow(u,v)  => astBins( t, "Pow", u, v )
      case Neg(u)    => astUnay( t, "Neg", u )
      case Abs(u)    => astUnay( t, "Abs", u )
      case Par(u)    => astUnay( t, "Par", u )
      case Brc(u)    => astUnay( t, "Brc", u )
      case Lnn(u)    => astFun1( t, "Lnn", u )
      case Log(u,r)  => astRoot( t, "Log", u, r )
      case Roo(u,r)  => astRoot( t, "Roo", u, r )
      case Eee(u)    => astFun1( t, "Eee", u )
      case Sqt(u)    => astFun1( t, "Sqt", u )
      case Sin(u)    => astFun1( t, "Sin",  u )
      case Cos(u)    => astFun1( t, "Cos",  u )
      case Tan(u)    => astFun1( t, "Tan",  u )
      case Csc(u)    => astFun1( t, "Csc",  u )
      case Sec(u)    => astFun1( t, "Sec",  u )
      case Cot(u)    => astFun1( t, "Cot",  u )
      case ASin(u)   => astFun1( t, "ASin", u )
      case ACos(u)   => astFun1( t, "ACos", u )
      case ATan(u)   => astFun1( t, "ATan", u )
      case ACsc(u)   => astFun1( t, "ACsc", u )
      case ASec(u)   => astFun1( t, "ASec", u )
      case ACot(u)   => astFun1( t, "ACot", u )
      case Dif(u)    => astFun1( t, "Dif",  u )
      case Itg(u)    => astFun1( t, "Itg",  u )
      case Sus(u,v)  => astBins( t, "Sus",  u, v )
      case Sup(u,v)  => astBins( t, "Sup",  u, v )
      case Lim(u,v)  => astBins( t, "Lim",  u, v )
      case Itl(a,b,u)=> astLims( t, "Itl",  a, b, u )
      case Sum(a,b,u)=> astLims( t, "Itg",  a, b, u )
      case Equ(u,v)  => astBins( t, "Equ",  u, v )
      case Cex(r,i)  => astCexx(t,r,i)
      case Vex(a)    => astVexx(t,a)
      case Mex(m)    => astMexx(t,m)
      case Msg(s)    => t.app(s)
    }
  }

  def astRec1( t:Text, name:String, u:Exp ): Unit =
    { t.all( name, '(' ); u.ast(t); t.app(')') }

  def astStr2( t:Text, name:String, s:String ): Unit =
    { t.all( name, '(', s, ')' ) }

  def astStr3( t:Text, name:String, s1:String, s2:String ): Unit =
    { t.all(name, '(', s1, ',', s2, ')') }

  def astUnay( t:Text, name:String, u:Exp ): Unit =
    { t.all( name, '(' ); u.ast(t); t.app(')') }

  def astBins( t:Text, name:String, u:Exp, v:Exp ): Unit =
    { t.all( name, '(' ); u.ast(t); t.app(','); v.ast(t); t.app(')') }

  def astRoot( t:Text, name:String, u:Exp, r:Dbl ): Unit =
    { t.all( name, '(' ); u.ast(t); t.app(','); t.all("Dbl(",r.r,")"); t.app(')') }

  def astFun1( t:Text, name:String, u:Exp ): Unit =
    { t.all( name, '('); u.ast(t); t.app(')') }

  def astFun2( t:Text, name:String, u:Exp, v:Exp ): Unit =
    { t all(name, '('); u.ast(t); t.app(','); v.ast(t); t.app(')') }
  
  def astLims( t:Text, name:String, a:Exp, b:Exp, u:Exp ): Unit =
    { t.all(name, '('); a.ast(t); t.app(','); b.ast(t); t.app(','); u.ast(t); t.app(')') }
  
  def astCexx( t:Text, r:Exp, i:Exp ): Unit =
    { t.app("Cex("); ast(t,r); t.app(','); ast(t,i); t.app(')') }
  
  def astVexx( t:Text, a:Array[Exp] ): Unit = {
    t.app("Vex("); a(0).ast(t)
    for( i <- 1 until a.length ) 
      { t.app(','); a(i).ast(t) }
    t.app( ')' )
  }
  
  def astMexx( t:Text, mat:Array[Vex] ): Unit = {
    t.all ("Mex(", mat(0).ast(t))
    for( i <- 1 until mat.length )
      { t.app(','); mat(i).ast(t) }
    t.app(')') 
  }
}
