
package ax6.math.exp

import  ax6.math.num.Vec

case class Vex( array:Array[Exp] ) extends Exp
{
  val n:Int = array.length
  
  def a(      i:Int ) : Exp = array(i)
  def apply(  i:Int ) : Exp = array(i)
  def update( i:Int, b:Exp ) : Unit = { array(i) = b }

  def + ( v:Vex ) : Vex = 
    { val u = Vex(n); for( i <- this ) { u(i) = Add(a(i),v(i)) }; u }
  def - ( v:Vex ) : Vex = 
    { val u = Vex(n); for( i <- this ) { u(i) = Sub(a(i),v(i)) }; u }

  def mul( s:Exp )         : Vex = // Exp * Vex  -- called by Exp
    { val u = Vex(n); for( i <- this ) { u(i) = Mul(s,a(i)) }; u }

  def * ( v:Vex ) : Vex = // Vex * Vex
    { val u = Vex(n); for( i <- this ) { u(i) = Mul(a(i),v(i)) }; u }

  override def * ( s:Exp ) : Vex = // Vex * Exp
    { val u = Vex(n); for( i <- this ) { u(i) = Mul(a(i),s) }; u }

  def * ( b:Mex ) : Vex = // Vex * Mex
  {
    if( n != b.n ) throw new Error()
    val c = Vex(b.m)
    for( j <- c )
    {
      c(j) = Mul(a(j),b(0,j))
      for( i <- 1 until n )
        c(j) = Add(c(j),Mul(a(j),b(i,j)))
    }
    c
  }

  def dot ( v:Vex ) : Exp = // Vex dot Vex = Exp scalar product
  { 
    val u:Vex = this * v
    var d:Exp = u(0)
    for( i <- 1 until n ) 
      d = Add( d, u(i) )
    d
  }   
           
  def mag  : Exp = 
    { var m:Exp = Num(0); for( i <- this ) { m = Add(m,Mul(a(i),a(i))) }; Sqt(m) }

  def Unit : Vex = 
    { val u = Vex(n); val s = Rec(mag); for( i <- this ) { u(i) = Mul(a(i),s) }; u; }

  def cos ( b:Vex ):Exp = Div( this dot b, Mul(mag,b.mag) )

// ... override dif, sim ...

  def calcVex( aa:Assign ) : Vec =
  {
    val vec = new Vec(n)
    for( i <- this )
      vec(i) = a(i).calc(aa)
    vec
  }

// ... for comprehensions ...

  def foreach( func:Int => Unit ): Unit = {
    var i = 0
    while( i < n )
      { func(i); i+=1 }
  } 

  def map( func:Exp => Exp ) : Vex =
  {
    val vex = Vex(n)
    for( i <- this )
        vex(i) = func( a(i) )
    vex
  } 

}

object Vex {

  def apply( _array:Array[Exp] ) : Vex = {
    val vex:Vex = new Vex( _array )
    for( i <- 0 until vex.n ) vex(i) = _array(i)
    vex
  }

  def apply( n:Int ) : Vex = {
    val array = new Array[Exp](n)
    Vex(array)
  }

  def apply( list:List[Exp] ) : Vex = {
    val vex:Vex = Vex( list.size )
    var i = 0
    for( e <- list ) { vex(i) = e; i = i + 1 }
    vex
  }

  def apply( args:Exp* ) : Vex = {
    val vex:Vex = Vex( args.length )
    var i = 0
    for( arg <- args ) { vex(i) = arg; i = i + 1 }
    vex
  }

  def apply( _vex:Vex ) : Vex = {
    val vex:Vex = Vex(_vex.n )
    for( i <- 0 until vex.n ) vex(i) = _vex(i)
    vex
  }

  def apply( exp:Exp ) : Vex = exp match  // This a cast
  {
    case vex:Vex  => vex
    case _        => Vex(0) // Logg.trace(4, "Bad Cast", exp.toString); emp
  }
}
