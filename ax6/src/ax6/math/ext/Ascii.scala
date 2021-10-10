package ax6.math.ext

import  ax6.math.exp._
import  ax6.util.Text
//import ax6.util.{Log => Logg}

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
      case Var(s)    => t.app( s )
      case Add(u,v)  => asciiAdd(  t, u, v, enc=false )
      case Mul(u,v)  => asciiMul(  t, u, v )
      case Sub(u,v)  => asciiSub(  t, u, v, enc=false )
      case Div(u,v)  => asciiDiv(  t, u, v )
      case Pow(u,v)  => asciiBin(  t, u, "^", v, enc=false )
      case Equ(u,v)  => asciiEqu(  t, u, v )
      case Rec(u)    => asciiRec(  t, u )
      case Neg(u)    => asciiMeg( t, u )
      case Abs(u)    => asciiEnc( t, "|", u, "|" )
      case Par(u)    => asciiEnc( t, "(", u, ")" )
      case Brc(u)    => asciiEnc( t, "{", u, "}" )
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
      case Itl(a,b,u) => asciiLim( t, "Int", a, b, u )
      case Sum(a,b,u) => asciiLim( t, "sum", a, b, u )
      case Cex(r,i)   => asciiCex(t,r,i)
      case Vex(array) => asciiVex(t,array)
      case Mex(mat)   => asciiMex(t,mat)
      case Msg(txt)   => t.app(txt)
    }

    exp match {
      case Par(_) => t.noop("None")
      case _      => t.delPar() }
  }

  def asciiBin( t:Text, u:Exp, op:String, v:Exp, enc:Boolean=true ) : Unit = {
    if( enc ) t.app('(')
    (u,v) match {
      case ( Add(a,b), Add(c,d) ) =>
        asciiAdd(t,a,b); t.app(op); asciiAdd(t,c,d);
      case ( Add(a,b), c:Exp  ) =>
        asciiAdd(t,a,b); t.app(op); c.ascii(t);
      case ( a:Exp,  Add(b,c) ) =>
        a.ascii(t); t.app(op);  asciiAdd(t,b,c);
      case ( Sub(a,b), Sub(c,d) ) =>
        asciiSub(t,a,b); t.app(op); asciiSub(t,c,d);
      case ( Sub(a,b), c:Exp  ) =>
        asciiSub(t,a,b); t.app(op); c.ascii(t);
      case ( a:Exp,  Sub(b,c) ) =>
        a.ascii(t); t.app(op);  asciiSub(t,b,c);  
      case ( a:Exp,  b:Exp  ) =>
        ascii(t,a); t.app(op); ascii(t,b);
    }
    if ( enc ) t.app(')')
  }

  def asciiAdd( t:Text, u:Exp, v:Exp, enc:Boolean=true ) : Unit = {
    if( enc ) t.app('(')
    (u,v) match {
      case ( Add(a,b), Add(c,d) ) =>
        a.ascii(t); t.app('+'); b.ascii(t); t.app('+'); c.ascii(t); t.app('+'); d.ascii(t);
      case ( Add(a,b), c:Exp  ) =>
        a.ascii(t); t.app('+'); b.ascii(t); t.app('+'); c.ascii(t);
      case ( a:Exp,  Add(b,c) ) =>
        a.ascii(t); t.app('+'); b.ascii(t); t.app('+'); c.ascii(t);
      case ( a:Exp,  b:Exp  ) =>
        a.ascii(t); t.app('+'); b.ascii(t);
    }
    if ( enc ) t.app(')')
  }


  def asciiSub( t:Text, u:Exp, v:Exp, enc:Boolean=true ) : Unit = {
    if( enc ) t.app('(')
    (u,v) match {
      case ( Sub(a,b), Sub(c,d) ) =>
        a.ascii(t); t.app('-'); b.ascii(t); t.app('-'); c.ascii(t); t.app("+"); d.ascii(t);
      case ( Sub(a,b), c:Exp  ) =>
        a.ascii(t); t.app('-'); b.ascii(t); t.app('-'); c.ascii(t);
      case ( a:Exp,  Sub(b,c) ) =>
        a.ascii(t); t.app('-'); b.ascii(t); t.app("+"); c.ascii(t);
      case ( a:Exp,  b:Exp  ) =>
        a.ascii(t); t.app('-'); b.ascii(t);
    }
    if ( enc ) t.app(')')
  }

  def asciiMul( t:Text, u:Exp, v:Exp ) : Unit = {
    // Logg.log( "asciiMul beg", u.toLambda, v.toLambda )
    (u, v) match {
      case (Mul(a, b), Mul(c, d)) =>
        a.ascii(t); t.app('*'); b.ascii(t); t.app('*'); c.ascii(t); t.app('*'); d.ascii(t);
      case (Mul(a, b), c: Exp) =>
        a.ascii(t); t.app('*'); b.ascii(t); t.app('*'); c.ascii(t);
      case (a: Exp, Mul(b, c)) =>
        a.ascii(t); t.app('*'); b.ascii(t); t.app('*'); c.ascii(t);
      case (a: Exp, b: Exp) =>
        // Logg.log( "acsiiMul end", u.toLambda, v.toLambda )
        asciiBin(t, a, "*", b, enc=false )
    }
  }

  def asciiDiv( t:Text, u:Exp, v:Exp ) : Unit = {
    (u, v) match {
      case( Var(s), b:Exp ) => t.app(s);              t.app('/'); asciiEnc(t,"(",b,")");
      case( Dif(_), b:Exp ) => u.ascii(t);            t.app('/'); asciiEnc(t,"(",b,")");
      case( a:Exp,  b:Exp ) => asciiEnc(t,"(",a,")"); t.app('/'); asciiEnc(t,"(",b,")"); }
  }
  
  def asciiEqu( t:Text, u:Exp, v:Exp ) : Unit = {
    u.ascii(t); t.app('='); v.ascii(t) }

  def asciiRec( t:Text, u:Exp ): Unit = u match {
    case Add(a,b) => t.app('1'); t.app('/'); asciiAdd( t, a, b )
    case Sub(a,b) => t.app('1'); t.app('/'); asciiSub( t, a, b )
    case a:Exp    => t.app('1'); t.app('/'); a.ascii(t)
  }

  def asciiEnc( t:Text, beg:String, u:Exp, end:String ) : Unit = {
    t.app(beg); u.ascii(t); t.app(end) }

  def asciiMeg( t:Text, u:Exp ): Unit = {
    if( t.tail() == '+' ) t.delTail()
    t.app('-')
    u.ascii(t) }

  // Function
  def asciiFun( t:Text, func:String, u:Exp ): Unit = { t.app(func); asciiEnc(t,"(",u,")");  }

  // Function subscript
  def asciiBase( t:Text, func:String, r:Double, u:Exp ): Unit =
      { t.all( func, '_', r ); asciiEnc(t,"(",u,")") }
   
  // Function subscript superscript
  def asciiLim( t:Text, func:String, low:Exp, up:Exp, u:Exp ): Unit =
     { t.all(func, '_'); low.ascii(t); t.app('^'); up.ascii(t); asciiEnc(t,"(",u,")")   }

   def asciiDif( t:Text, u:Exp ): Unit = {
     u match
     {
       case Var(s)  => t.all( 'd',  s  )
       case _       => t.app( 'd'); asciiEnc(t,"(",u,")")
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
    case Add(u,v)   => asciiAdd( t, u,v )
    case Sub(u,v) => u.ascii(t); t.app('-'); v.ascii(t)
    //case _        => q.ascii(t)
  }

  // Optional paren for denominator to compensate for Simplify which strips Paren
  def asciiDenom( t:Text, q:Exp ): Unit = q match {
    case Mul(u,v) => asciiMul( t, u, v )
    case _ => asciiGroup(t, q)
  }
}