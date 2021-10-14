
package ax6.math.ext

import ax6.math.exp._
import ax6.util.{Log => Logg}

import scala.collection.mutable.ListBuffer

trait Simplify {
  self: Exp =>

  def sim: Exp = sim(this)

  // Removes all Par(u) parenthesis
  def sim(exp: Exp): Exp = exp match {
    case Par(u) => simplify(u)
    case a: Exp => simplify(a)
  }

  def simplify(exp: Exp): Exp = exp match {
    case Par(u) => sim(u)
    case Num(_) => exp
    case Var(_) => exp
    case Dbl(d) => simDbl(d)
    case Rat(n, d) => simRat(n, d)
    case Rec(u) => simRec(sim(u))
    case Add(u, v) => simAdd(sim(u), sim(v))
    case Mul(u, v) => simMul(sim(u), sim(v))
    case Sub(u, v) => simSub(sim(u), sim(v))
    case Div(u, v) => simDiv(sim(u), sim(v))
    case Pow(u, v) => simPow(sim(u), sim(v))
    case Equ(u, v) => simEqu(sim(u), sim(v))
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
    case Sus(u, v) => Sus(sim(u), sim(v))
    case Sup(u, v) => Sup(sim(u), sim(v))
    case Lim(u, v) => Lim(sim(u), sim(v))
    case Itg(u) => Itg(sim(u))
    case Itl(a, b, u) => Itl(sim(a), sim(b), sim(u))
    case Sum(a, b, u) => Sum(sim(a), sim(b), sim(u))
    case Cex(r, i) => Cex(sim(r), sim(i))
    case Vex(v) => Vex(v).map(e => e.sim)
    case Mex(m) => Mex(m).map(e => e.sim)
    case Msg(s) => Msg(s)
  }

  def simDbl(r: Double): Exp = if (r == r.toInt) Num(r.toInt) else Dbl(r)

  def simRat(n: Int, d: Int): Exp = (n, d) match {
    case (0, _) => Num(0)
    case (_, 0) => Msg("Num(n1)/Num(0)")
    case _ => if (n == d) Num(1) else Rat(n, d)
  }

  def simRec(u: Exp): Exp = u match {
    case Num(1) => 1
    case Num(0) => Msg("Rec(Num(0))")
    case Par(a) => Rec(sim(a))
    case _ => Rec(sim(u))
  }

  def simAdd(u: Exp, v: Exp): Exp = (u, v) match {
    case (q: Exp, Num(0) | Dbl(0.0)) => sim(q)
    case (Num(0) | Dbl(0.0), r) => sim(r)
    case (Num(a), Num(b)) => Num(a + b)
    case (Num(a), Dbl(b)) => Dbl(a + b)
    case (Dbl(a), Num(b)) => Dbl(a + b)
    case (Dbl(a), Dbl(b)) => Dbl(a + b)
    case (q: Exp, Neg(b)) => sim(sim(q) - sim(b))
    case _ => Add(sim(u), sim(v))
  }

  def simMul(u: Exp, v: Exp): Exp = (u, v) match {
    case (a: Exp, Num(1) | Dbl(1.0)) => sim(a)
    case (Num(1) | Dbl(1.0), v1) => sim(v1)
    case (Num(a), Num(b)) => Num(a * b)
    case (Num(a), Dbl(b)) => Dbl(a * b)
    case (Dbl(a), Num(b)) => Dbl(a * b)
    case (Dbl(a), Dbl(b)) => Dbl(a * b)
    case (Div(a, b), Div(c, d)) => factor(Mul(a, b), Mul(c, d))
    case (a: Exp, Div(b, c)) => factor(Mul(a, b), c)
    case (Div(a, b), c: Exp) => factor(Mul(a, c), b)
    case _ => Mul(sim(u), sim(v))
  }

  def simSub(u: Exp, v: Exp): Exp = (u, v) match {
    case (q: Exp, Num(0) | Dbl(0.0)) => sim(q)
    case (Num(0) | Dbl(0.0), r) => sim(Neg(r))
    case (Num(a), Num(b)) => Num(a - b)
    case (Num(a), Dbl(b)) => Dbl(a - b)
    case (Dbl(a), Num(b)) => Dbl(a - b)
    case (Dbl(a), Dbl(b)) => Dbl(a - b)
    case (q: Exp, Neg(b)) => Add(sim(q), sim(b))
    case _ => if (u == v) Num(0) else Sub(sim(u), sim(v))
  }

  def simDiv(u: Exp, v: Exp): Exp = (u, v) match {
    case (a: Exp, Num(1) | Dbl(1.0)) => sim(a)
    case (_: Exp, Num(0) | Dbl(0.0)) => Msg("Divide by 0")
    case (Num(a), Num(b)) => Rat(a, b)
    case (Num(a), Dbl(b)) => Dbl(a / b)
    case (Dbl(a), Num(b)) => Dbl(a / b)
    case (Dbl(a), Dbl(b)) => Dbl(a / b)
    case (Pow(a, b), Pow(c, d)) => if (u == v) Num(1) else facPow(a, b, c, d)
    case (a: Exp, b: Exp) =>
      Logg.log("simDiv", "a:" + a.toAscii, "b:" + b.toAscii)
      factor(a, b)
  }

  def simPow(u: Exp, v: Exp): Exp = (u, v) match {
    case (a: Exp, Num(1) | Dbl(1.0)) => sim(a)
    case (_, Num(0) | Dbl(0.0)) => 1
    case (Num(1) | Dbl(1.0), _) => 1
    case (Num(0) | Dbl(0.0), _) => 0
    case (Num(a), Num(b)) => Dbl(Math.pow(a, b))
    case (Num(a), Dbl(b)) => Dbl(Math.pow(a, b))
    case (Dbl(a), Num(b)) => Dbl(Math.pow(a, b))
    case (Dbl(a), Dbl(b)) => Dbl(Math.pow(a, b))
    case (a: Exp, Sub(b, c)) => if (b == c) Num(1) else Pow(sim(a), Sub(sim(a), sim(b)))
    case _ => Pow(sim(u), sim(v))
  }

  def simEqu(u: Exp, v: Exp): Exp = (u, v) match {
    case _ => Equ(sim(u), sim(v))
  }

  def facPow(b1: Exp, p1: Exp, b2: Exp, p2: Exp): Exp = {
    if (p1 == p2 && b1 == b2) Num(1)
    if (b1 == b2) Pow(sim(b1), Sub(sim(p1), sim(p2)))
    else Div(Pow(sim(b1), sim(p1)), Pow(sim(b2), sim(p2)))
  }

  def factor( uExp: Exp, vExp: Exp): Exp = {
    val aList = toList(uExp)
    val bList = toList(vExp)
    val fList = new ListBuffer[Exp]()
    val gList = new ListBuffer[Exp]()
    val uList = new ListBuffer[Exp]()
    val vList = new ListBuffer[Exp]()
    Logg.log("factor abLists", aList, bList )
    for( a <- aList ) { if( in(a,bList,1) )  fList += a }
    for( b <- bList ) { if( in(b,aList,1) )  gList += b }
    for( a <- aList ) { if( in(a,fList,0) )  uList += a }
    for( b <- bList ) { if( in(b,gList,0) )  vList += b }
    Logg.log("factor uvLists", uList, vList, fList, gList )
    Logg.log("factor expMuls", toExps(uList), toExps(vList))
    
    if(uList.isEmpty && vList.isEmpty ) Num(1)
    else if (uList.isEmpty) Rec(toExps(vList))
    else if (vList.isEmpty) toExps(uList)
    else Div(toExps(uList), toExps(vList))
  }

  def in( exp:Exp, list:ListBuffer[Exp], count:Int ) : Boolean = {
    val isIn = list.count( elem => elem == exp ) == count

    //if( isIn && del==0 ) {
    //  Logg.log("factor in", exp, list )
    //  list.update( list.indexOf[Exp](exp), Num(1) ) }
    isIn
  }

  def toList(exp: Exp): ListBuffer[Exp] = {
    val list: ListBuffer[Exp] = new ListBuffer[Exp]()
    recurse(exp, list)
    list
  }

  def toExps(list: ListBuffer[Exp]): Exp = {
    if( list.isEmpty ) return Num(1)
    val head = list.head
    val tail = list.tail
    if (list.lengthCompare(1) > 0) Mul(head, toExps(tail)) else head
  }

  def recurse(exp: Exp, list: ListBuffer[Exp]): Unit = {

    var next: Exp = exp match {
      case Mul(u, v) => recurse(u, list); v
      case u: Exp => list += u; null
    }

    while (next != null) {
      recurse(next, list)
      next = next match {
        case Mul(u, v) => recurse(u, list); v
        case _: Exp => null
      }
    }

  }

}

/*
    for( a <- aList ) { if(  bList.contains(a) )  fList += a }
    for( a <- aList ) { if( !fList.contains(a) )  uList += a }
    for( b <- bList ) { if( !fList.contains(b) )  vList += b }

  def remove( exp:Exp, uList:ListBuffer[Exp], vList:ListBuffer[Exp] ) : Unit = {
    val iu = uList.indexOf(exp)
    val iv = vList.indexOf(exp)
    if( iv > 0 ) uList.remove( iu )
    if( iv > 0 ) vList.remove( iv ) }

  def factorOut( uList:ListBuffer[Exp], vList:ListBuffer[Exp] ) : Unit = {
    val fList:ListBuffer[Exp] = new ListBuffer[Exp]()
    for( u <- uList;
         v <- vList if u == v ) yield fList += u

    for( exp <- fList ) {
      val iu = uList.indexOf(exp)
      val iv = vList.indexOf(exp)
      if( iv > 0 ) uList.remove( iu )
      if( iv > 0 ) vList.remove( iv ) }
  }

  def factorOut2( uList:ListBuffer[Exp], vList:ListBuffer[Exp] ) : Unit = {
    val fList:ListBuffer[Exp] = new ListBuffer[Exp]()
    for( exp <- uList ) {
      if( uList.contains[Exp](exp) && vList.contains[Exp](exp) ) {
        fList += exp } }

    for( exp <- fList ) {
      val iu = uList.indexOf(exp)
      val iv = vList.indexOf(exp)
      if( iv > 0 ) uList.remove( iu )
      if( iv > 0 ) vList.remove( iv ) }
  }

    for( a <- aList; b <- bList if a == b ) yield fList += a
    for( a <- aList; f <- fList if a != f ) yield uList += a
    for( b <- bList; f <- fList if b != f ) yield vList += b

    for( a <- aList ) { if( bList.contains(a) ) remove( a, uList, vList ) }
    for( b <- aList ) { if( bList.contains(a) ) remove( a, uList, vList ) }
 */