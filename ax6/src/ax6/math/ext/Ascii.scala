package ax6.math.ext

import  ax6.math.exp._
import  ax6.util.Text

trait Ascii
{
  self:Exp =>

  def toAscii : String = {
    val t:Text = new Text(100)
    ascii( t )
    t.toStr
  }
     
  def ascii( t:Text ): Unit =
    { ascii(t,this) }
  
  def ascii( t:Text, exp:Exp ): Unit =
  { exp match
    {
      case Num(n)    => t.app( n.toString )
      case Dbl(r)    => t.app( r.toString )
      case Rat(n,d)  => t.all(n.toString, '/', d.toString)
      case Var(s)    => t.app( s ) // t.app( Syms.sym(s) )
      case Add(u)    => asciilist( t, "+", u )
      case Sub(u,v)  => asciiBin( t, "-", u, v )
      case Mul(u)    => asciilist( t, "+", u )
      case Div(u,v)  => asciiBin( t, "/", u, v )
      case Pow(u,v)  => asciiBin( t, "^", u, v )
      case Equ(u,v)  => asciiBin( t, "=", u, v )
      case Rec(u)    => t.app("1"); t.app('/'); u.ascii(t)
      case Neg(u)    => t.app('-');  u.ascii(t)
      case Abs(u)    => t.app('|');  u.ascii(t); t.app('|')
      case Par(u)    => t.app('(');  u.ascii(t); t.app(')')
      case Brc(u)    => t.app('{');  u.ascii(t); t.app('}')
      case Eee(u)    => t.app("e^"); u.ascii(t)
      case Lnn(u)    => asciiFun( t, "ln",     u )
      case Sqt(u)    => asciiFun( t, "sqrt",   u )
      case Sin(u)    => asciiFun( t, "sin",    u )
      case Cos(u)    => asciiFun( t, "cos",    u )
      case Tan(u)    => asciiFun( t, "tan",    u )
      case Csc(u)    => asciiFun( t, "csc",    u )
      case Sec(u)    => asciiFun( t, "sec",    u )
      case Cot(u)    => asciiFun( t, "cot",    u )
      case ASin(u)   => asciiFun( t, "arcsin", u )
      case ACos(u)   => asciiFun( t, "arccos", u )
      case ATan(u)   => asciiFun( t, "arctan", u )
      case ACsc(u)   => asciiFun( t, "arccsc", u )
      case ASec(u)   => asciiFun( t, "arcsec", u )
      case ACot(u)   => asciiFun( t, "arccot", u )
      case Itg(u)    => asciiFun( t, "Int",    u )
      case Dif(u)    => asciiDif(t,u)
      case Sus(u,v)  => u.ascii(t); t.app('_'); v.ascii(t)
      case Sup(u,v)  => u.ascii(t); t.app('^'); v.ascii(t)
      case Log(u,b)  => asciiBase( t, "Log",  b.r, u )
      case Roo(u,b)  => asciiBase( t, "root", b.r, u )
      case Lim(u,v)  => t.app('_'); u.ascii(t); t.app('^'); v.ascii(t)
      case Itl(a,b,u)=> asciiLim( t, "Int", a, b, u )
      case Sum(a,b,u)=> asciiLim( t, "sum", a, b, u )
      case Cex(r,i)  => asciiCex(t,r,i)
      case Vex(a)    => asciiVex(t,a)
      case Mex(mat)  => asciiMex(t,mat)
      case Msg(txt)  => t.app(txt)
    }
  }

  def asciiParen( t:Text, u:Exp ): Unit = {
    t.app('('); u.ascii(t); t.app(')') }

  def asciiBin( t:Text, op:String, u:Exp, v:Exp ) : Unit = {
    if( t.len!=0 && ( op=="+" || op=="-" ) ) {
      t.app('('); u.ascii(t); t.app(op); v.ascii(t); t.app(')') }
    else {
      u.ascii(t); t.app(op); v.ascii(t) }
  }

  def asciilist( t:Text, op:String, exps:List[Exp] ): Unit = {
    val enc = t.len!=0 && ( op=="+" || op=="-" )
    if( enc ) t.app('(')
    for( exp <- exps )
      { exp.ascii(t); t.app(op) }
    t.delTail()
    if( enc ) t.app(')')
  }

  // Function
  def asciiFun( t:Text, func:String, u:Exp ): Unit = { t.app(func); asciiParen(t,u);  }

  // Function subscript
  def asciiBase( t:Text, func:String, r:Double, u:Exp ): Unit =
      { t.all( func, '_', r ); asciiParen(t,u) }
   
  // Function subscript superscript
  def asciiLim( t:Text, func:String, low:Exp, up:Exp, u:Exp ): Unit =
     { t.all(func, '_'); low.ascii(t); t.app('^'); up.ascii(t); asciiParen(t,u)   }
  
   def asciiDif( t:Text, u:Exp ): Unit = {
     u match
     {
       case Var(s) if s.length==1 => t.all('d', s)
       case _                     => asciiFun( t, "d", u )
     }
   }
   
   def asciiCex( t:Text, r:Exp, i:Exp ): Unit =
     { t.app('['); ascii(t,r); t.app(','); ascii(t,i); t.app(".i]") }
  
   def asciiVex( t:Text, a:Array[Exp] ): Unit = {
     t.app('[')
     a(0).ascii(t)
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

  // Optional paren for Add Sub to compensate for Simplify which strips Paren
  def asciiGroup( t:Text, q:Exp ): Unit = q match {
    case Add(u)   => asciilist( t, "+", u )
    case Sub(u,v) => u.ascii(t); t.app('-'); v.ascii(t)
    //case _        => q.ascii(t)
  }

  // Optional paren for denominator to compensate for Simplify which strips Paren
  def asciiDenom( t:Text, q:Exp ): Unit = q match {
    case Mul(u) => asciilist( t, "*", u )
    case _ => asciiGroup(t, q)
  }


}