
package ax6.math.ext

import ax6.math.exp._
// import ax6.util.{Log => Logg}

import scala.collection.mutable.ListBuffer

trait Simplify {
  self:Exp =>
  type LB = ListBuffer[Exp]

  def sim:Exp = sim(this)

  // Removes all Par(u) parenthesis
  def sim(exp:Exp) : Exp = exp match {
    case Par(u) => simplify(u)
    case a:Exp => simplify(a)
  }

  def simplify(exp:Exp) : Exp = exp match {
    case Par(u) => sim(u)
    case Num(_) => exp
    case Var(_) => exp
    case Dbl(d) => simDbl(d)
    case Rat(n,d) => simRat(n, d)
    case Rec(u) => simRec(sim(u))
    case Add(u,v) => simAdd(sim(u), sim(v))
    case Mul(u,v) => simMul(sim(u), sim(v))
    case Sub(u,v) => simSub(sim(u), sim(v))
    case Div(u,v) => simDiv(sim(u), sim(v))
    case Pow(u,v) => simPow(sim(u), sim(v))
    case Equ(u,v) => simEqu(sim(u), sim(v))
    case Neg(u) => Neg(sim(u))
    case Abs(u) => Abs(sim(u))
    case Brc(u) => Brc(sim(u))
    case Lnn(u) => Lnn(sim(u))
    case Log(u, r) => Log(sim(u), r)
    case Roo(u, r) => Roo(sim(u), r)
    case Eee(u) => Eee(sim(u))
    case Sqt(u) => Sqt(sim(u))
    case Sin(u) => Sin(sim(u))
    case Cos(u) => Cos(sim(u))
    case Tan(u) => Tan(sim(u))
    case Csc(u) => Csc(sim(u))
    case Sec(u) => Sec(sim(u))
    case Cot(u) => Cot(sim(u))
    case ASin(u) => ASin(sim(u))
    case ACos(u) => ACos(sim(u))
    case ATan(u) => ATan(sim(u))
    case ACsc(u) => ACsc(sim(u))
    case ASec(u) => ASec(sim(u))
    case ACot(u) => ACot(sim(u))
    case Dif(u) => Dif(sim(u))
    case Sus(u,v) => Sus(sim(u), sim(v))
    case Sup(u,v) => Sup(sim(u), sim(v))
    case Lim(u,v) => Lim(sim(u), sim(v))
    case Itg(u) => Itg(sim(u))
    case Itl(a,b,u) => Itl(sim(a), sim(b), sim(u))
    case Sum(a,b,u) => Sum(sim(a), sim(b), sim(u))
    case Cex(r, i) => Cex(sim(r), sim(i))
    case Vex(v) => Vex(v).map(e => e.sim)
    case Mex(m) => Mex(m).map(e => e.sim)
    case Msg(s) => Msg(s)
  }

  def simDbl(r:Double) : Exp = if( r == r.toInt) Num(r.toInt) else Dbl(r)

  def simRat(n:Int, d:Int) : Exp = (n, d) match {
    case (0, _) => Num(0)
    case (_, 0) => Msg("Num(n1)/Num(0)")
    case _ => if( n == d) Num(1) else Rat(n, d)
  }

  def simRec(u:Exp) : Exp = u match {
    case Num(1) => 1
    case Num(0) => Msg("Rec(Num(0))")
    case Par(a) => Rec(sim(a))
    case _ => Rec(sim(u))
  }

  def simAdd(u:Exp, v:Exp) : Exp = (u, v) match {
    case (q:Exp, Num(0) | Dbl(0.0)) => sim(q)
    case (Num(0) | Dbl(0.0), r) => sim(r)
    case (Num(a), Num(b)) => Num(a + b)
    case (Num(a), Dbl(b)) => Dbl(a + b)
    case (Dbl(a), Num(b)) => Dbl(a + b)
    case (Dbl(a), Dbl(b)) => Dbl(a + b)
    case (q:Exp, Neg(b)) => sim(sim(q) - sim(b))
    case _ => Add(sim(u), sim(v))
  }

  def simMul(u:Exp, v:Exp) : Exp = (u, v) match {
    case (a:Exp, Num(1) | Dbl(1.0)) => sim(a)
    case (Num(1) | Dbl(1.0), v1) => sim(v1)
    case (Num(a), Num(b)) => Num(a * b)
    case (Num(a), Dbl(b)) => Dbl(a * b)
    case (Dbl(a), Num(b)) => Dbl(a * b)
    case (Dbl(a), Dbl(b)) => Dbl(a * b)
    case (Div(a, b), Div(c, d)) => factor(Mul(a, b), Mul(c, d))
    case (a:Exp, Div(b, c)) => factor(Mul(a, b), c)
    case (Div(a, b), c:Exp) => factor(Mul(a, c), b)
    case _ => Mul(sim(u), sim(v))
  }

  def simSub(u:Exp, v:Exp) : Exp = (u, v) match {
    case (q:Exp, Num(0) | Dbl(0.0)) => sim(q)
    case (Num(0) | Dbl(0.0), r) => sim(Neg(r))
    case (Num(a), Num(b)) => Num(a - b)
    case (Num(a), Dbl(b)) => Dbl(a - b)
    case (Dbl(a), Num(b)) => Dbl(a - b)
    case (Dbl(a), Dbl(b)) => Dbl(a - b)
    case (q:Exp, Neg(b)) => Add(sim(q), sim(b))
    case _ => if( u == v) Num(0) else Sub(sim(u), sim(v))
  }

  def simDiv(u:Exp, v:Exp) : Exp = (u, v) match {
    case (a:Exp, Num(1) | Dbl(1.0)) => sim(a)
    case (_:Exp, Num(0) | Dbl(0.0)) => Msg("Divide by 0")
    case (Num(a), Num(b)) => Rat(a, b)
    case (Num(a), Dbl(b)) => Dbl(a / b)
    case (Dbl(a), Num(b)) => Dbl(a / b)
    case (Dbl(a), Dbl(b)) => Dbl(a / b)
    case (Pow(a, b), Pow(c, d)) => if( u == v) Num(1) else facPow(a, b, c, d)
    case (a:Exp, b:Exp) =>
      // Logg.log("simDiv", "a:" + a.toAscii, "b:" + b.toAscii)
      factor(a, b)
  }

  def simPow(u:Exp, v:Exp) : Exp = (u, v) match {
    case (a:Exp, Num(1) | Dbl(1.0)) => sim(a)
    case (_, Num(0) | Dbl(0.0)) => 1
    case (Num(1) | Dbl(1.0), _) => 1
    case (Num(0) | Dbl(0.0), _) => 0
    case (Num(a), Num(b)) => Dbl(Math.pow(a, b))
    case (Num(a), Dbl(b)) => Dbl(Math.pow(a, b))
    case (Dbl(a), Num(b)) => Dbl(Math.pow(a, b))
    case (Dbl(a), Dbl(b)) => Dbl(Math.pow(a, b))
    case (a:Exp, Sub(b, c)) => if( b == c) Num(1) else Pow(sim(a), Sub(sim(a), sim(b)))
    case _ => Pow(sim(u), sim(v))
  }

  def simEqu(u:Exp, v:Exp) : Exp = (u, v) match {
    case _ => Equ(sim(u), sim(v))
  }

  def facPow(b1:Exp, p1:Exp, b2:Exp, p2:Exp) : Exp = {
    if( p1 == p2 && b1 == b2) Num(1)
    if( b1 == b2) Pow(sim(b1), Sub(sim(p1), sim(p2)))
    else Div(Pow(sim(b1), sim(p1)), Pow(sim(b2), sim(p2)))
  }

  def factor( uExp:Exp, vExp:Exp) : Exp = {
    
    val aList = toList(uExp)
    val bList = toList(vExp)
    var fList = new LB()
    var gList = new LB()
    var mList = new LB() 
    val uList = new LB()
    val vList = new LB()

    // Create lists of common factors then choose the
    // mininmum set then clo ne it so factors can be
    // removed foor bookeeping once a factor has been applied
    for( a <- aList ) { if( in(a,bList) ) fList += a }
    for( b <- bList ) { if( in(b,aList) ) gList += b }
    mList = minList( fList, gList )
    fList = mList.clone()
    gList = mList.clone()

    // Build new numberator 'uList' and denomuator 'vList' list
    // without the common factors then remove the common factors
    for( a <- aList ) { if( inUpdate(a,fList) ) uList += a }
    for( b <- bList ) { if( inUpdate(b,gList) ) vList += b }

    // Generate the final factored expression output
    if( uList.isEmpty && vList.isEmpty ) Num(1)
    else if(  uList.isEmpty) Rec(toExps(vList))
    else if(  vList.isEmpty) toExps(uList)
    else Div( toExps(uList), toExps(vList) )
  }

  def in( exp:Exp, list:LB ) :Boolean = {
    list.contains( exp )
  }

  // Determine if a factor is in a list and if so effectively remove it
  // by setting it to the multiplicative identity Num(1)
  def inUpdate( fac:Exp, list:LB ) :Boolean = {
    val isIn = in( fac, list )
    if( isIn) {
      list.update( list.indexOf[Exp](fac), Num(1) ) }
    !isIn
  }

  def minList( fList:LB, gList:LB ) :LB = {
    if( fList.lengthCompare(gList.size) <= 0 ) fList else gList
  }

  def toList( exp:Exp) : LB = {
    val list:LB = new LB()
    recurse(exp, list)
    list
  }

  def toExps( list:LB) : Exp = {
    if( list.isEmpty ) return Num(1)
    val head = list.head
    val tail = list.tail
    if( list.lengthCompare(1) > 0 ) Mul(head, toExps(tail)) else head
  }

  def recurse( exp:Exp, list:LB) : Unit = {
    var next:Exp = exp
    while( next != null ) {
      next = next match {
        case Mul(u,v) => recurse(u,list); v
        case u:Exp    => list += u; null
      }
    }

  }

}
