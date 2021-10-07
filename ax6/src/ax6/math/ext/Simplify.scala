
package ax6.math.ext

import ax6.math.exp._
import ax6.util.Text
import ax6.util.{Log => Logg}

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
    case Add(u)    => simAdd(u)
    case Mul(u)    => simMul(u)
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

  def simAdd(  list1:List[Exp] ) : Exp = Add(list1).map( exp => sim(exp) )
  def simAdd2( list1:List[Exp] ) : Exp = {
    val list2 = copyBuff(list1)
    for(   u <- list1 ) {
      for( v <- list2 ) {
        binAdd( u, v ) match {
          case Sub(a,b) => list2 :+ a; list2 :+ b;  // Sub is a binary Add here
          case exp:Exp  => list2 :+ exp } } }
    toAdd( Add( list2.toList ) )
  }

  def simMul(  list1:List[Exp] ) : Exp = Mul(list1).map( exp => sim(exp) )

  def simMul2( list1:List[Exp] ) : Exp = {
    val list2 = makeBuff()
    for(   u <- list1 ) {
      for( v <- list2 ) {
        binMul( u, v ) match {
          case Sub(a,b) => list2 += a; list2 += b;  // Sub is a binary Mul here
          case exp:Exp  => list2 += exp } } }
    toMul( Mul( list2.toList ) )
  }

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
    case( Mul(a), Mul(b) ) => facMul( a, b )
    case( a:Exp,  Mul(b) ) => facTop( a, b )
    case( Mul(a), b:Exp  ) => facBot( a, b )
    case( Add(a), Add(b) ) => facAdd( a, b )
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
        Logg.typ( "simDiv fail a", a )
        Logg.typ( "simDiv fail b", a )
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
    case( a:Exp,    Add(b)    ) => Add(b).map( e => Mul(sim(a),sim(e)) )
    case( Add(a),   b:Exp     ) => Add(a).map( e => Mul(sim(e),sim(b)) )
    case( a:Exp,    Sub(b,c)  ) => Sub( Mul(sim(a),sim(b)), Mul(sim(a),sim(c)) )
    case( Sub(a,b), c:Exp     ) => Sub( Mul(sim(a),sim(c)), Mul(sim(b),sim(c)) )
    case _                      => Mul(sim(u),sim(v))
  }

  def toAdd( add:Add ) : Exp = if( add.list.isEmpty ) Num(0) else sim(add)
  def toMul( mul:Mul ) : Exp = if( mul.list.isEmpty ) Num(1) else sim(mul)

  def delExp( exp:Exp, src:List[Exp] ) : List[Exp] = {
    val list2 = makeBuff()
    for(  elem <- src ) {
      if( elem != exp  ) {
        list2 += elem } }
    list2.toList
  }

  def facPow( b1:Exp, p1:Exp, b2:Exp, p2:Exp ) : Exp =
  {
    if( p1==p2 && b1==b2 ) Num(1)
    if( b1==b2           ) Pow( sim(b1), Sub(sim(p1),sim(p2)) )
    else Div( Pow(sim(b1),sim(p1)), Pow(sim(b2),sim(p2)) )
  }

  def facAdd( u:List[Exp], v:List[Exp]  ) : Exp = {
    if( u == v ) Num(1) else Div( Add(u), Add(v) ) }

  def facMul( listu:List[Exp], listv:List[Exp]  ) : Exp = {
    Logg.log( "facMul beg", listu, listv )
    val lista = makeBuff()
    val listb = makeBuff()
    for( u <- listu ) { if( !listv.contains(u) ) { lista += u } }
    for( v <- listv ) { if( !listu.contains(v) ) { listb += v } }
    Div( toMul(Mul(lista.toList)), toMul(Mul(listb.toList)) )
  }

  def facTop( u:Exp, listv:List[Exp] ) : Exp = {
    val listb = delExp( u, listv )
    if( listb.contains( u ) ) Rec(Mul(listb)) else Div( sim(u), toMul(Mul(listb)) )
  }

  def facBot( listu:List[Exp], v:Exp ) : Exp = {
    val lista = delExp( v, listu )
    if( lista.contains( v ) ) Mul(lista) else Div( toMul(Mul(lista)), sim(v) )
  }
}