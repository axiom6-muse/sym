
package ax6.math.ext

import ax6.math.exp._

trait Differentiate
{
  self:Exp =>
  
  def dif : Exp = d(this)  // this.dif
  
  def d( exp:Exp ) : Exp = exp match
  {
    case Num(_)    => 0
    case Dbl(_)    => 0
    case Rat(_,_)  => 0
    case Var(s)    => Dif(Var(s))
    case Add(u,v)  => d(u) + d(v)
    case Sub(u,v)  => d(u) - d(v)
    case Mul(u,v)  => difMul(u,v)                       // v*d(u)+u*d(v)
    case Div(u,v)  => (v*d(u)-u*d(v)) / v~^2
    case Rec(u)    => -d(u) / u~^2
    case Pow(u,v)  => difPow(u,v)
    case Neg(u)    => -d(u)
    case Abs(u)    => Abs(d(u))
    case Par(u)    => Par(d(u))
    case Brc(u)    => Brc(d(u))
    case Lnn(u)    => d(u)/u
    case Log(u,r)  => Log(Ee,r)*d(u)/u
    case Roo(u,r)  => d(u) / Par( r * Roo(u,r) )
    case Eee(u)    => Eee(u) * d(u)
    case Sqt(u)    => d(u) / Par( Sqt(u) * 2 )
    case Sin(u)    =>   Cos(u) * d(u)
    case Cos(u)    => -(Sin(u) * d(u))
    case Tan(u)    => -(Sec(u)~^2  * d(u))
    case Csc(u)    => -(Csc(u)*Cot(u) * d(u))
    case Sec(u)    =>   Sec(u)*Tan(u) * d(u)
    case Cot(u)    => -(Csc(u)~^2  * d(u))
    case ASin(u)   =>   d(u)/Sqt( 1 - u~^2 )
    case ACos(u)   => -(d(u)/Sqt( 1 - u~^2 ))
    case ATan(u)   =>   d(u)/( 1 + u~^2 )
    case ACot(u)   => -(d(u)/( 1 + u~^2 ))
    case ACsc(u)   => -(d(u)/(u*Sqt( u~^2 - 1)))
    case ASec(u)   =>   d(u)/(u*Sqt( u~^2 - 1))
    case Equ(u,v)  => d(u) equ d(v)
    case Dif(u)    => Dif(Dif(u))
    case Itg(u)    => u
    case Itl(q,b,u)=> Itl(q,b,d(u))
    case Sum(q,b,u)=> Sum(q,b,d(u))
    case Cex(r,i)  => Cex( r.dif, i.dif )
    case Vex(v)    => Vex(v).map( e => e.dif )
    case Mex(m)    => Mex(m).map( e => e.dif )
    case Msg(s)    => Dif(Msg(s))
 // case Sus(u,v)  => Dif(Sub(u,v))
 // case Sup(u,v)  => Dif(Sup(u,v))
 // case Lim(u,v)  => Dif(Lim(u,v))
  }

  def difMul( u:Exp, v:Exp ) : Exp = (u,v) match
  {
    case ( Mul(a,b), Mul(c,q) ) => b*c*q*d(a) + a*c*q*d(b)  + a*b*q*d(c) + a*b*c*d(q)
    case ( Mul(a,b), c:Exp    ) => b*c*d(a)   + a*c*d(b)    + a*b*d(c)
    case ( a:Exp,    Mul(b,c) ) => b*c*d(a)   + a*c*d(b)    + a*b*d(c)
    case ( a:Exp,    b:Exp    ) => b*d(a)     + a*d(b)
  }

  def difPow( u:Exp, v:Exp ) : Exp = (u,v) match
  {
    case ( _,  Num(0) ) => 0
    case ( u1, Num(1) ) => d(u1)
    case ( u1, Num(2) ) => 2 * u1 * d(u)
    case ( u1, Num(n) ) => n * u1~^(n-1) * d(u1)
    case ( u1, Dbl(r) ) => r *  1~^(r-1) * d(u1)
    case ( Num(n), v1 ) => ln(n) * n~^v1 * d(v1)
    case ( Dbl(r), v1 ) => ln(r) * r~^v1 * d(v1)
    case _              => v * u~^(v-1)  * d(u) + Lnn(u) * u~^v * d(v)
  }

}