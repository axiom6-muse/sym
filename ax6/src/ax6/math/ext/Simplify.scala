
package ax6.math.ext

import  ax6.math.exp._
import  ax6.util.Text

//import  core.topo.{ Hold, Hode }

trait Simplify
{
  self:Exp =>
  
  def sim : Exp = sim(this)
  
  def sim( exp:Exp ) : Exp = exp match
  {
    case Num(_)    => exp
    case Dbl(d)    => dbl(d)
    case Rat(n,d)  => rat(n,d)
    case Var(_)    => exp
    case Add(u,v)  => add(noparen(u),noparen(v))
    case Sub(u,v)  => sub(noparen(u),noparen(v))
    case Mul(u,v)  => mul(noparen(u),noparen(v))
    case Div(u,v)  => div(noparen(u),noparen(v))    
    case Pow(u,v)  => pow(noparen(u),noparen(v))
    case Rec(u)    => rec(u)
    case Neg(u)    => Neg(sim(u))
    case Abs(u)    => Abs(sim(u))
    case Par(u)    => Par(sim(u))
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
    case Equ(u,v)  => equ(u,v)
    case Dif(u)    => Dif(sim(u))
    case Sus(u,v)  => Sus(sim(u),sim(v))
    case Sup(u,v)  => Sup(sim(u),sim(v))
    case Lim(u,v)  => Lim(sim(u),sim(v))
    case Itg(u)    => Itg(sim(u)) 
    case Itl(a,b,u)=> Itl(sim(a),sim(b),sim(u))  
    case Sum(a,b,u)=> Sum(sim(a),sim(b),sim(u))   
    case Cex(r,i)  => Cex( r.sim, i.sim )
    case Vex(v)    => Vex(v).map( e => e.sim )
    case Mex(m)    => Mex(m).map( e => e.sim )
    case Msg(t)    => Msg(t)
  }

  def dbl( r:Double ) : Exp = if(r==r.toInt) Num(r.toInt) else Dbl(r)

  def rat( n:Int, d:Int ) : Exp = (n,d) match
  {
    case( 0, _ ) => Num(0)
    case( _, 0 ) => Msg( Text("Num(n1)/Num(0)") )
    case _       => if(n==d) 1 else Rat(n,d)
  }

  def mul( u:Exp, v:Exp ) : Exp = (u,v) match
  {
    case( u1, Num(1)|Dbl(1.0) ) => sim(u1)
    case( Num(1)|Dbl(1.0), v1 ) => sim(v1)
    case( Num(a),   Num(b)    ) => Num(a*b)
    case( Num(a),   Dbl(b)    ) => Dbl(a*b)
    case( Dbl(a),   Num(b)    ) => Dbl(a*b)
    case( Dbl(a),   Dbl(b)    ) => Dbl(a*b)
    case( q,        Add(a,b)  ) => sim( sim(q)*sim(a)+sim(q)*sim(b) )
    case( Add(a,b), r         ) => sim( sim(a)*sim(r)+sim(b)*sim(b) )
    case( q,        Sub(a,b)  ) => sim( sim(q)*sim(a)-sim(u)*sim(b) )
    case( Sub(a,b), r         ) => sim( sim(a)*sim(r)-sim(b)*sim(b) )
    case _                      => sim(u)*sim(v)
  }

  // Stack recursion needs to be reexamined
  def div( u:Exp, v:Exp ) : Exp = (u,v) match
  {
    case( q, Num(1)|Dbl(1.0) ) => sim(q)
    case( _, Num(0)|Dbl(0.0) ) => Msg(Text("u/Num(0)"))
    case( Num(a),   Num(b)   ) => Rat(a,b)
    case( Num(a),   Dbl(b)   ) => Dbl(a/b)
    case( Dbl(a),   Num(b)   ) => Dbl(a/b)
    case( Dbl(a),   Dbl(b)   ) => Dbl(a/b) 
    case( Mul(a,b), denom    ) => div(mul(a,b),sim(denom))
  //case( numer, Mul(a,b)    ) => div(sim(numer),mul(a,b))
    case _                     => u / v
  }

  def add( u:Exp, v:Exp ) : Exp = (u,v) match
  {
    case( q, Num(0)|Dbl(0.0) ) => sim(q)
    case( Num(0)|Dbl(0.0), r ) => sim(r)
    case( Num(a),   Num(b)   ) => Num(a+b)
    case( Num(a),   Dbl(b)   ) => Dbl(a+b)
    case( Dbl(a),   Num(b)   ) => Dbl(a+b)
    case( Dbl(a),   Dbl(b)   ) => Dbl(a+b)     
    case( q,        Neg(b)   ) => sim( sim(q)-sim(b) )
    case _                     =>      sim(u)+sim(v)
  }

  def sub( u:Exp, v:Exp ) : Exp = (u,v) match
  {
    case( q, Num(0)|Dbl(0.0) ) => sim(q)
    case( Num(0)|Dbl(0.0), r ) => sim( Neg(r) )
    case( Num(a),   Num(b)   ) => Num(a-b)
    case( Num(a),   Dbl(b)   ) => Dbl(a-b)
    case( Dbl(a),   Num(b)   ) => Dbl(a-b)
    case( Dbl(a),   Dbl(b)   ) => Dbl(a-b)    
    case( q,        Neg(b)   ) => sim( sim(q)+sim(b) )
    case _                     => sim(u)-sim(v)
  }

  def pow( u:Exp, v:Exp ) : Exp = (u,v) match
  {
    case( q, Num(1)|Dbl(1.0) ) => sim(q)
    case( _, Num(0)|Dbl(0.0) ) => 1
    case( Num(1)|Dbl(1.0), _ ) => 1
    case( Num(0)|Dbl(0.0), _ ) => 0
    case( Num(a),   Num(b)   ) => Dbl(Math.pow(a,b))
    case( Num(a),   Dbl(b)   ) => Dbl(Math.pow(a,b))
    case( Dbl(a),   Num(b)   ) => Dbl(Math.pow(a,b))
    case( Dbl(a),   Dbl(b)   ) => Dbl(Math.pow(a,b))    
    case _                     => sim(u)~^sim(v)
  }

  def rec( u:Exp ) : Exp = u match
  {
    case Num(1) => 1
    case Num(0) => Msg(Text("Rec(Num(0))"))
    case Par(a) => Rec(sim(a))
    case _      => Rec(sim(u))
  }

  def equ( u:Exp, v:Exp ) : Exp = (u,v) match
  {
    case _ => Equ( sim(u), sim(v) )
  }
}
