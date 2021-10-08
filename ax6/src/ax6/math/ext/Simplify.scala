
package ax6.math.ext

import ax6.math.exp._
import ax6.util.Text
//import ax6.util.{Log => Logg}

trait Simplify
{
  self:Exp =>
  
  def sim : Exp = sim(this)

  // Removes all Par(u) parenthesis
  def sim( exp:Exp ) : Exp = exp match {
    case Par(u) => simplify(u)
    case a:Exp  => simplify(a)
  }
  
  def simplify( exp:Exp ) : Exp = exp match
  {
    case Par(u)    => sim(u)
    case Num(_)    => exp
    case Var(_)    => exp
    case Dbl(d)    => simDbl(d)
    case Rat(n,d)  => simRat(n,d)
    case Rec(u)    => simRec(sim(u))
    case Add(u,v)  => simAdd(sim(u),sim(v))
    case Mul(u,v)  => simMul(sim(u),sim(v))
    case Sub(u,v)  => simSub(sim(u),sim(v))
    case Div(u,v)  => simDiv(sim(u),sim(v))
    case Pow(u,v)  => simPow(sim(u),sim(v))
    case Equ(u,v)  => simEqu(sim(u),sim(v))
    case Neg(u)    => Neg(sim(u))
    case Abs(u)    => Abs(sim(u))
    case Brc(u)    => Brc(sim(u))
    case Lnn(u)    => Lnn(sim(u))
    case Log(u,r)  => Log(sim(u),r)
    case Roo(u,r)  => Roo(sim(u),r)
    case Eee(u)    => Eee(sim(u))
    case Sqt(u)    => Sqt(sim(u))
    case Sin(u)    => Sin(sim(u))
    case Cos(u)    => Cos(sim(u))
    case Tan(u)    => Tan(sim(u))
    case Csc(u)    => Csc(sim(u))
    case Sec(u)    => Sec(sim(u))
    case Cot(u)    => Cot(sim(u))
    case ASin(u)   => ASin(sim(u))
    case ACos(u)   => ACos(sim(u))
    case ATan(u)   => ATan(sim(u))
    case ACsc(u)   => ACsc(sim(u))
    case ASec(u)   => ASec(sim(u))
    case ACot(u)   => ACot(sim(u))
    case Dif(u)    => Dif(sim(u))
    case Sus(u,v)  => Sus(sim(u),sim(v))
    case Sup(u,v)  => Sup(sim(u),sim(v))
    case Lim(u,v)  => Lim(sim(u),sim(v))
    case Itg(u)    => Itg(sim(u)) 
    case Itl(a,b,u)=> Itl(sim(a),sim(b),sim(u))  
    case Sum(a,b,u)=> Sum(sim(a),sim(b),sim(u))   
    case Cex(r,i)  => Cex(sim(r),sim(i) )
    case Vex(v)    => Vex(v).map( e => e.sim )
    case Mex(m)    => Mex(m).map( e => e.sim )
    case Msg(t)    => Msg(t)
  }

  def simDbl( r:Double ) : Exp = if(r==r.toInt) Num(r.toInt) else Dbl(r)

  def simRat( n:Int, d:Int ) : Exp = (n,d) match
  {
    case( 0, _ ) => Num(0)
    case( _, 0 ) => Msg( Text("Num(n1)/Num(0)") )
    case _       => if(n==d) 1 else Rat(n,d)
  }

  def simRec( u:Exp ) : Exp = u match
  {
    case Num(1) => 1
    case Num(0) => Msg(Text("Rec(Num(0))"))
    case Par(a) => Rec(sim(a))
    case _      => Rec(sim(u))
  }

  def simAdd(  u:Exp, v:Exp ) : Exp = Add(u,v)


  def simMul(  u:Exp, v:Exp  ) : Exp = Mul(u,v)


  def simSub( u:Exp, v:Exp ) : Exp = (u,v) match
  {
    case( q:Exp, Num(0)|Dbl(0.0) ) => sim(q)
    case( Num(0)|Dbl(0.0), r ) => sim( Neg(r) )
    case( Num(a),   Num(b)   ) => Num(a-b)
    case( Num(a),   Dbl(b)   ) => Dbl(a-b)
    case( Dbl(a),   Num(b)   ) => Dbl(a-b)
    case( Dbl(a),   Dbl(b)   ) => Dbl(a-b)
    case( q:Exp,    Neg(b)   ) => Add(sim(q),sim(b))
    case _                     => Sub(sim(u),sim(v))
  }

  def simDiv( u:Exp, v:Exp ) : Exp = (u,v) match
  {
    case( Mul(a,b), Mul(c,d) ) => facMul( a, b, c, d )
    case( a:Exp,    Mul(b,c) ) => facTop( a, b, c )
    case( Mul(a,b), c:Exp    ) => facBot( a, b, c )
    case( a:Exp,  Num(1) | Dbl(1.0) ) => sim(a)
    case( _:Exp,  Num(0) | Dbl(0.0) ) => Msg(Text("u/Num(0)")) // Divide by 0
    case( Num(a), Num(b) ) => Rat(a,b)
    case( Num(a), Dbl(b) ) => Dbl(a/b)
    case( Dbl(a), Num(b) ) => Dbl(a/b)
    case( Dbl(a), Dbl(b) ) => Dbl(a/b)
    case( Pow(a,b), Pow(c,d) ) => facPow( a, b, c, d )
    case( a:Exp,  b:Exp  ) =>
      if( a==b )  Num(1)
      else {
        //Logg.typ( "simDiv fail a", a )
        //Logg.typ( "simDiv fail b", a )
        Div(sim(a),sim(b)) }
  }

  def simPow( u:Exp, v:Exp ) : Exp = (u,v) match
  {
    case( a:Exp, Num(1)|Dbl(1.0) ) => sim(a)
    case( _, Num(0)|Dbl(0.0)     ) => 1
    case( Num(1)|Dbl(1.0), _     ) => 1
    case( Num(0)|Dbl(0.0), _     ) => 0
    case( Num(a),     Num(b)     ) => Dbl(Math.pow(a,b))
    case( Num(a),     Dbl(b)     ) => Dbl(Math.pow(a,b))
    case( Dbl(a),     Num(b)     ) => Dbl(Math.pow(a,b))
    case( Dbl(a),     Dbl(b)     ) => Dbl(Math.pow(a,b))
    case _                         => Pow(sim(u),sim(v))
  }

  def simEqu( u:Exp, v:Exp ) : Exp = (u,v) match
  {
    case _ => Equ( sim(u), sim(v) )
  }

  def binAdd( u:Exp, v:Exp ) : Exp = (u,v) match
  {
    case( q:Exp, Num(0)|Dbl(0.0) ) => sim(q)
    case( Num(0)|Dbl(0.0), r )     => sim(r)
    case( Num(a),   Num(b)   )     => Num(a+b)
    case( Num(a),   Dbl(b)   )     => Dbl(a+b)
    case( Dbl(a),   Num(b)   )     => Dbl(a+b)
    case( Dbl(a),   Dbl(b)   )     => Dbl(a+b)
    case( q:Exp,    Neg(b)   )     => sim( sim(q)-sim(b) )
    case _                         => Sub(sim(u),sim(v))    // Sub is a binary Add here
  }

  def binMul( u:Exp, v:Exp ) : Exp = (u,v) match
  {
    case( a:Exp, Num(1)|Dbl(1.0) ) => sim(a)
    case( Num(1)|Dbl(1.0), v1 ) => sim(v1)
    case( Num(a),   Num(b)    ) => Num(a*b)
    case( Num(a),   Dbl(b)    ) => Dbl(a*b)
    case( Dbl(a),   Num(b)    ) => Dbl(a*b)
    case( Dbl(a),   Dbl(b)    ) => Dbl(a*b)
    case( a:Exp,    Add(b,c)  ) => Add( Mul(sim(a),sim(b)), Mul(sim(a),sim(c)) )
    case( Add(a,b), c:Exp     ) => Add( Mul(sim(a),sim(c)), Mul(sim(b),sim(c)) )
    case( a:Exp,    Sub(b,c)  ) => Sub( Mul(sim(a),sim(b)), Mul(sim(a),sim(c)) )
    case( Sub(a,b), c:Exp     ) => Sub( Mul(sim(a),sim(c)), Mul(sim(b),sim(c)) )
    case _                      => Mul(sim(u),sim(v))
  }

  def facPow( b1:Exp, p1:Exp, b2:Exp, p2:Exp ) : Exp =
  {
    if( p1==p2 && b1==b2 ) Num(1)
    if( b1==b2           ) Pow( sim(b1), Sub(sim(p1),sim(p2)) )
    else Div( Pow(sim(b1),sim(p1)), Pow(sim(b2),sim(p2)) )
  }


  def facMul( a:Exp, b:Exp, c:Exp, d:Exp  ) : Exp = {
    if(      a == c ) b / d
    else if( b == c ) a / d
    else if( a == d ) b / c
    else if( b == d ) a / c
    else Div(Mul(a,b),Mul(c,d))
  }

  def facTop( a:Exp, b:Exp, c:Exp ) : Exp = {
    if(      a == b ) Rec(c)
    else if( a == c ) Rec(b)
    else Div(a,Mul(b,c))
  }

  def facBot( a:Exp, b:Exp, c:Exp ) : Exp = {
    if(      a == c ) b
    else if( b == c ) a
    else Div(Mul(a,b),c)
  }
}