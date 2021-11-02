package ax6.math.ext

import  ax6.math.exp._
import  ax6.util.Text

// Not implemented yet

trait Latex
{
  self:Exp =>

  def toLatex : String = {
    val t:Text = new Text(100)
    latex( t )
    t.toStr
  }
     
  def latex(  t:Text ): Unit = {  latex(t,this) }

  def latex( t:Text, exp:Exp ): Unit = {
     exp match
     {
      case Num(n)    => t.app( n.toString )
      case Dbl(r)    => t.app( r.toString )
      case Rat(n,d)  => t.all( n.toString, '/', d.toString )
      case Var(s)    => t.app( s ) // t.app( Syms.sym(s) )
      case Add(u,v)  => u.latex(t); t.app('+'); v.latex(t)
      case Sub(u,v)  => u.latex(t); t.app('-'); v.latex(t)
      case Mul(u,v)  => u.latex(t); t.app('*'); v.latex(t)
      case Div(u,v)  => asciiGroup(t,u); t.app('/'); asciiDenom(t,v)
      case Rec(u)  => t.app("1"); t.app('/'); asciiGroup(t,u)
      case Pow(u,v)  => asciiGroup(t,u); t.app('^'); asciiGroup(t,v)
      case Neg(u)    => t.app('-'); u.latex(t)
      case Abs(u)    => asciiEnc(  t,"|",u,"|")
      case Par(u)    => asciiEnc(  t,"(",u,")")
      case Brc(u)    => asciiEnc(  t,"[",u,"]")
      case Lnn(u)    => latexFun(  t, "ln", u )
      case Log(u,b)  => latexRoot( t, "log", b.r, u )
      case Roo(u,r)  => latexRoot( t, "root",r.r, u )
      case Eee(u)    => t.app("e^"); asciiGroup(t,u)
      case Sqt(u)    => latexFun( t, "sqrt",   u )
      case Sin(u)    => latexFun( t, "sin",    u )
      case Cos(u)    => latexFun( t, "cos",    u )
      case Tan(u)    => latexFun( t, "tan",    u )
      case Csc(u)    => latexFun( t, "csc",    u )
      case Sec(u)    => latexFun( t, "sec",    u )
      case Cot(u)    => latexFun( t, "cot",    u )
      case ASin(u)   => latexFun( t, "arcsin", u )
      case ACos(u)   => latexFun( t, "arccos", u )
      case ATan(u)   => latexFun( t, "arctan", u )
      case ACsc(u)   => latexFun( t, "arccsc", u )
      case ASec(u)   => latexFun( t, "arcsec", u )
      case ACot(u)   => latexFun( t, "arccot", u )
      case Equ(u,v)  => u.latex(t); t.app('='); v.latex(t)
      case Dif(u)    => latexDif(t,u)
      case Sus(u,v)  => u.latex(t); t.app('_'); v.latex(t)
      case Sup(u,v)  => u.latex(t); t.app('^'); v.latex(t)
      case Lim(u,v)  => t.app('_'); u.latex(t); t.app('^'); v.latex(t)
      case Itg(u)    => latexFun( t, "Int", u )
      case Itl(a,b,u)=> latexSum( t, "Int", a, b, u )
      case Sum(a,b,u)=> latexSum( t, "sum", a, b, u )
      case Cex(r,i)  => latexCex(t,r,i)
      case Vex(a)    => latexVex(t,a)
      case Mex(mat)  => latexMex(t,mat)
      case Msg(s) => t.app(s)
    }
   }
  
  // Function
  def latexFun( t:Text, func:String, u:Exp ): Unit =
     { t.app(func); asciiEnc(t,"(",u,")") }

  // Log and Root base
   def latexRoot( t:Text, func:String, r:Double, u:Exp ): Unit =
     { t.all(func,'_',r); asciiEnc(t,"(",u,")") }
   
  // Function subscript superscript
  def latexSum( t:Text, func:String, a:Exp, b:Exp, u:Exp ): Unit =
     { t.all(func,'_'); a.latex(t); t.app('^'); b.latex(t); asciiEnc(t,"(",u,")") }
  
  def latexDif( t:Text, u:Exp ): Unit = {
     u match
     {
       case Var(s) =>
         if( s.length==1 )
           t.all( 'd', s )
         else
           t.all( "d(", s, ')' )
        case _ =>
           t.app("d("); u.latex(t); t.app(")")
     }
   }
  
   def latexCex( t:Text, r:Exp, i:Exp ): Unit =
     { t.app('['); latex(t,r); t.app(','); latex(t,i); t.app("\\ii]") }
  
  def latexVex( t:Text, a:Array[Exp] ): Unit = {
    t.app( "\\begin{bmatrix}" ); a(0).latex(t)
    for( i <- 1 until a.length ) 
      { t.app(" & "); a(i).latex(t) }
    t.app( "\\end{bmatrix}" )
  }
   
  def latexMex( t:Text, mat:Array[Vex] ): Unit = {
    t.app( "\\begin{bmatrix}" )
    for( i <- mat.indices)
    { 
      mat(i)(0).latex(t)
      for( j <- 1 until mat(i).n )
        { t.app(" & "); mat(i)(j).latex(t) }
      if( i < mat.length-1 ) t.app("\\\\")
    }
    t.app( "\\end{bmatrix}" )
  }
}