package ax6.math.ext

import  ax6.math.exp._
import  ax6.util.Text

trait Ascii
{
  self:Exp =>
     
  def ascii( t:Text ): Unit =
    { ascii(t,this) }
  
  def ascii( t:Text, exp:Exp ): Unit =
  { exp match
    {
      case Num(n)    => t.app( n.toString )
      case Dbl(r)    => t.app( r.toString )
      case Rat(n,d)  => t.all(n.toString, '/', d.toString)
      case Var(s)    => t.app( s ) // t.app( Syms.sym(s) )
      case Add(u,v)  => u.ascii(t); t.app('+'); v.ascii(t)
      case Sub(u,v)  => u.ascii(t); t.app('-'); v.ascii(t)
      case Mul(u,v)  => u.ascii(t); t.app('*'); v.ascii(t)
      case Div(u,v)  => u.ascii(t); t.app('/'); v.ascii(t)
      case Rec(u)    => t.app("1"); t.app('/'); u.ascii(t)
      case Pow(u,v)  => u.ascii(t); t.app('^'); v.ascii(t)
      case Neg(u)    => t.app('-'); u.ascii(t)
      case Abs(u)    => t.app('|'); u.ascii(t); t.app('|')
      case Par(u)    => t.app('('); u.ascii(t); t.app(')')
      case Brc(u)    => t.app('{'); u.ascii(t); t.app('}')
      case Lnn(u)    => ascii( t, "ln", u )
      case Log(u,b)  => ascii( t, "log", b.r, u )
      case Roo(u,r)  => ascii( t, "root",r.r, u )
      case Eee(u)    => t.app("e^"); u.ascii(t)
      case Sqt(u)    => ascii( t, "sqrt",   u )
      case Sin(u)    => ascii( t, "sin",    u )
      case Cos(u)    => ascii( t, "cos",    u )
      case Tan(u)    => ascii( t, "tan",    u )
      case Csc(u)    => ascii( t, "csc",    u )
      case Sec(u)    => ascii( t, "sec",    u )
      case Cot(u)    => ascii( t, "cot",    u )
      case ASin(u)   => ascii( t, "arcsin", u )
      case ACos(u)   => ascii( t, "arccos", u )
      case ATan(u)   => ascii( t, "arctan", u )
      case ACsc(u)   => ascii( t, "arccsc", u )
      case ASec(u)   => ascii( t, "arcsec", u )
      case ACot(u)   => ascii( t, "arccot", u )
      case Equ(u,v)  => u.ascii(t); t.app('='); v.ascii(t)
      case Dif(u)    => asciiDif(t,u)
      case Sus(u,v)  => u.ascii(t); t.app('_'); v.ascii(t)
      case Sup(u,v)  => u.ascii(t); t.app('^'); v.ascii(t)
      case Lim(u,v)  => t.app('_'); u.ascii(t); t.app('^'); v.ascii(t)
      case Itg(u)    => ascii( t, "Int", u )
      case Itl(a,b,u)=> ascii( t, "Int", a, b, u )
      case Sum(a,b,u)=> ascii( t, "sum", a, b, u )
      case Cex(r,i)  => asciiCex(t,r,i)
      case Vex(a)    => asciiVex(t,a)
      case Mex(mat)  => asciiMex(t,mat)
      case Msg(txt)  => t.app(txt)
    }
  }

   // Optional paren for Add Sub to compensate for Simplify which strips Paren
   def group( t:Text, q:Exp ): Unit = q match {
     case Add(u,v) => u.ascii(t); t.app('+'); v.ascii(t)
     case Sub(u,v) => u.ascii(t); t.app('-'); v.ascii(t)
   //case _        => q.ascii(t)
   }

// Optional paren for denominator to compensate for Simplify which strips Paren
  def denom( t:Text, q:Exp ): Unit = q match {
    case Mul(u, v) => u.ascii(t); t.app('*'); v.ascii(t)
    case _ => group(t, q)
  }
  def paren( t:Text, u:Exp ): Unit = {
    t.app('('); u.ascii(t); t.app(')') }
     
// Binary Operation
// def ascii( t:Text, u:Exp, oper:String, v:Exp ) : Unit =
//     { u.ascii(t); t.app(oper); v.ascii(t)  }     

// Function
   def ascii( t:Text, func:String, u:Exp ): Unit = { t.app(func); paren(t,u);  }

// Function subscript
   def ascii( t:Text, func:String, r:Double, u:Exp ): Unit =
      { t.all(func, '_', r); paren(t,u) }
   
// Function subscript superscript
   def ascii( t:Text, func:String, a:Exp, b:Exp, u:Exp ): Unit =
     { t.all(func, '_'); a.ascii(t); t.app('^'); b.ascii(t); paren(t,u)   }
  
   def asciiDif( t:Text, u:Exp ): Unit = {
     u match
     {
       case Var(s) if s.length==1 => t.all('d', s)
       case _                     => ascii( t, "d", u )
     }
   }
   
   def asciiCex( t:Text, r:Exp, i:Exp ): Unit =
     { t.app('['); ascii(t,r); t.app(','); ascii(t,i); t.app(".i]") }
  
   def asciiVex( t:Text, a:Array[Exp] ): Unit = {
     t.app('['); a(0).ascii(t)
      for( i <- 1 until a.length ) 
        { t.app( ',' ); a(i).ascii(t) }
     t.app( ']' )
   }
   
   def asciiMex( t:Text, mat:Array[Vex] ): Unit = {
    t.app( '[' )
    for( i <- mat.indices)
      mat(i).ascii(t)
    t.app( ']' )
  }
  
}