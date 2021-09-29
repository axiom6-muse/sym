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
      case Add(u,v)  => binOp( t, "+", u, v )
      case Sub(u,v)  => binOp( t, "-", u, v )
      case Mul(u,v)  => u.ascii(t); t.app('*'); v.ascii(t)
      case Div(u,v)  => u.ascii(t); t.app('/'); v.ascii(t)
      case Rec(u)    => t.app("1"); t.app('/'); u.ascii(t)
      case Pow(u,v)  => u.ascii(t); t.app('^'); v.ascii(t)
      case Equ(u,v)  => u.ascii(t); t.app('='); v.ascii(t)
      case Neg(u)    => t.app('-'); u.ascii(t)
      case Adds(list) => asciLlist( t, '+', list )
      case Muls(list) => asciLlist( t, '*', list )
      case Abs(u)    => t.app('|'); u.ascii(t); t.app('|')
      case Par(u)    => t.app('('); u.ascii(t); t.app(')')
      case Brc(u)    => t.app('{'); u.ascii(t); t.app('}')
      case Eee(u)    => t.app("e^"); u.ascii(t)
      case Lnn(u)    => fun( t, "ln",     u )
      case Sqt(u)    => fun( t, "sqrt",   u )
      case Sin(u)    => fun( t, "sin",    u )
      case Cos(u)    => fun( t, "cos",    u )
      case Tan(u)    => fun( t, "tan",    u )
      case Csc(u)    => fun( t, "csc",    u )
      case Sec(u)    => fun( t, "sec",    u )
      case Cot(u)    => fun( t, "cot",    u )
      case ASin(u)   => fun( t, "arcsin", u )
      case ACos(u)   => fun( t, "arccos", u )
      case ATan(u)   => fun( t, "arctan", u )
      case ACsc(u)   => fun( t, "arccsc", u )
      case ASec(u)   => fun( t, "arcsec", u )
      case ACot(u)   => fun( t, "arccot", u )
      case Itg(u)    => fun( t, "Int",    u )
      case Dif(u)    => dif(t,u)
      case Sus(u,v)  => u.ascii(t); t.app('_'); v.ascii(t)
      case Sup(u,v)  => u.ascii(t); t.app('^'); v.ascii(t)
      case Log(u,b)  => base( t, "Log",  b.r, u )
      case Roo(u,b)  => base( t, "root", b.r, u )
      case Lim(u,v)  => t.app('_'); u.ascii(t); t.app('^'); v.ascii(t)
      case Itl(a,b,u)=> lim( t, "Int", a, b, u )
      case Sum(a,b,u)=> lim( t, "sum", a, b, u )
      case Cex(r,i)  => cex(t,r,i)
      case Vex(a)    => vex(t,a)
      case Mex(mat)  => mex(t,mat)
      case Msg(txt)  => t.app(txt)
    }
  }

  def paren( t:Text, u:Exp ): Unit = {
    t.app('('); u.ascii(t); t.app(')') }

  // t.tail() == '(' is a week condition for detecting detect that a binOp is already enclose by parens
  def binOp( t:Text, op:String, u:Exp, v:Exp ) : Unit = {
    if( t.tail() == '(' ) {
      u.ascii(t); t.app(op); v.ascii(t) }
    else {
      t.app('('); u.ascii(t); t.app(op); v.ascii(t); t.app(')') }
  }

  // Function
  def fun( t:Text, func:String, u:Exp ): Unit = { t.app(func); paren(t,u);  }

  // Function subscript
  def base( t:Text, func:String, r:Double, u:Exp ): Unit =
      { t.all( func, '_', r ); paren(t,u) }
   
  // Function subscript superscript
  def lim( t:Text, func:String, low:Exp, up:Exp, u:Exp ): Unit =
     { t.all(func, '_'); low.ascii(t); t.app('^'); up.ascii(t); paren(t,u)   }
  
   def dif( t:Text, u:Exp ): Unit = {
     u match
     {
       case Var(s) if s.length==1 => t.all('d', s)
       case _                     => fun( t, "d", u )
     }
   }
   
   def cex( t:Text, r:Exp, i:Exp ): Unit =
     { t.app('['); ascii(t,r); t.app(','); ascii(t,i); t.app(".i]") }
  
   def vex( t:Text, a:Array[Exp] ): Unit = {
     t.app('[')
     a(0).ascii(t)
     for( i <- 1 until a.length )
       { t.app( ',' ); a(i).ascii(t) }
     t.app( ']' )
   }

    def asciLlist( t:Text, op:Char, exps:List[Exp] ): Unit = {
      for( exp <- exps )
        { exp.ascii(t); t.app(op) }
      t.delTail()
    }
   
   def mex( t:Text, mat:Array[Vex] ): Unit = {
    t.app( '[' )
    for( i <- mat.indices)
      mat(i).ascii(t)
    t.app( ']' )
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

  
}