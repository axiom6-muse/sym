
package ax6.math.ext

import  ax6.math.exp._
import  ax6.util.Text

trait Lambda
{
  self:Exp =>

  def toLambda : String = {
    val t:Text = new Text(100)
    lambda( t )
    t.toStr
  }
    
  def lambda( t:Text ): Unit = { lambda(t,this) }
  
  def lambda( t:Text, exp:Exp ): Unit = {
    exp match
    {
      case Num(n)    => lambdaStr2( t, "Num", n.toString )
      case Dbl(r)    => lambdaStr2( t, "Dbl", r.toString )
      case Rat(n,d)  => lambdaStr3( t, "Rat", n.toString, d.toString )
      case Var(s)    => lambdaStr2( t, "Var", s )
      case Add(u,v)  => lambdaBins( t, "Add", u, v )
      case Sub(u,v)  => lambdaBins( t, "Sub", u, v )
      case Mul(u,v)  => lambdaBins( t, "Mul", u, v )
      case Div(u,v)  => lambdaBins( t, "Div", u, v )
      case Rec(u)    => lambdaBins( t, "Rec", Num(1), u )
      case Pow(u,v)  => lambdaBins( t, "Pow", u, v )
      case Neg(u)    => lambdaUnay( t, "Neg", u )
      case Abs(u)    => lambdaUnay( t, "Abs", u )
      case Par(u)    => lambdaUnay( t, "Par", u )
      case Brc(u)    => lambdaUnay( t, "Brc", u )
      case Lnn(u)    => lambdaFun1( t, "Lnn", u )
      case Log(u,r)  => lambdaRoot( t, "Log", u, r )
      case Roo(u,r)  => lambdaRoot( t, "Roo", u, r )
      case Eee(u)    => lambdaFun1( t, "Eee", u )
      case Sqt(u)    => lambdaFun1( t, "Sqt", u )
      case Sin(u)    => lambdaFun1( t, "Sin",  u )
      case Cos(u)    => lambdaFun1( t, "Cos",  u )
      case Tan(u)    => lambdaFun1( t, "Tan",  u )
      case Csc(u)    => lambdaFun1( t, "Csc",  u )
      case Sec(u)    => lambdaFun1( t, "Sec",  u )
      case Cot(u)    => lambdaFun1( t, "Cot",  u )
      case ASin(u)   => lambdaFun1( t, "ASin", u )
      case ACos(u)   => lambdaFun1( t, "ACos", u )
      case ATan(u)   => lambdaFun1( t, "ATan", u )
      case ACsc(u)   => lambdaFun1( t, "ACsc", u )
      case ASec(u)   => lambdaFun1( t, "ASec", u )
      case ACot(u)   => lambdaFun1( t, "ACot", u )
      case Dif(u)    => lambdaFun1( t, "Dif",  u )
      case Itg(u)    => lambdaFun1( t, "Itg",  u )
      case Sus(u,v)  => lambdaBins( t, "Sus",  u, v )
      case Sup(u,v)  => lambdaBins( t, "Sup",  u, v )
      case Lim(u,v)  => lambdaBins( t, "Lim",  u, v )
      case Itl(a,b,u)=> lambdaLims( t, "Itl",  a, b, u )
      case Sum(a,b,u)=> lambdaLims( t, "Itg",  a, b, u )
      case Equ(u,v)  => lambdaBins( t, "Equ",  u, v )
      case Cex(r,i)  => lambdaCexx(t,r,i)
      case Vex(a)    => lambdaVexx(t,a)
      case Mex(m)    => lambdaMexx(t,m)
      case Msg(txt:Text) => t.app(txt)
    }
  }

  def lambdaStr2( t:Text, name:String, s:String ): Unit =
    { t.all( name, '(', s, ')' ) }

  def lambdaStr3( t:Text, name:String, s1:String, s2:String ): Unit =
    { t.all(name, '(', s1, ',', s2, ')') }

  def lambdaUnay( t:Text, name:String, u:Exp ): Unit =
    { t.all( name, '(' ); u.lambda(t); t.app(')') }

  def lambdaBins( t:Text, name:String, u:Exp, v:Exp ): Unit =
    { t.all( name, '(' ); u.lambda(t); t.app(','); v.lambda(t); t.app(')') }

  def lambdaRoot( t:Text, name:String, u:Exp, r:Dbl ): Unit =
    { t.all( name, '(' ); u.lambda(t); t.app(','); t.all("Dbl(",r.r,")"); t.app(')') }

  def lambdaFun1( t:Text, name:String, u:Exp ): Unit =
    { t.all( name, '('); u.lambda(t); t.app(')') }

  def lambdaFun2( t:Text, name:String, u:Exp, v:Exp ): Unit =
    { t all(name, '('); u.lambda(t); t.app(','); v.lambda(t); t.app(')') }
  
  def lambdaLims( t:Text, name:String, a:Exp, b:Exp, u:Exp ): Unit =
    { t.all(name, '('); a.lambda(t); t.app(','); b.lambda(t); t.app(','); u.lambda(t); t.app(')') }
  
  def lambdaCexx( t:Text, r:Exp, i:Exp ): Unit =
    { t.app("Cex("); lambda(t,r); t.app(','); lambda(t,i); t.app(')') }
  
  def lambdaVexx( t:Text, a:Array[Exp] ): Unit = {
    t.app("Vex("); a(0).lambda(t)
    for( i <- 1 until a.length ) 
      { t.app(','); a(i).lambda(t) }
    t.app( ')' )
  }
  
  def lambdaMexx( t:Text, mat:Array[Vex] ): Unit = {
    t.all ("Mex(", mat(0).lambda(t))
    for( i <- 1 until mat.length )
      { t.app(','); mat(i).lambda(t) }
    t.app(')') 
  }
}
