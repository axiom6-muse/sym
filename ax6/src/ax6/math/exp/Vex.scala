
package ax6.math.exp

import  ax6.math.num.Vec
import  ax6.util.{  Log=>Logg }

case class Vex( a:Array[Exp] ) extends Exp  
{   
  val n : Int = a.length
  
  def this( na:Int ) = this( new Array[Exp](na) )

  def this( args : Exp* ) =
    this( new Array[Exp]( args.length ) )
    var i=0
    for( arg<-args )
      this.a(i) = arg
      i=i+1


  def this( list : List[Exp] ) =
    this( new Array[Exp]( list.size ) )
    var i=0
    for( arg<-list ) {
      this.a(i) = arg
      i=i+1; }


  def this( vex:Vex ) =
    this( new Array[Exp]( vex.n ) )
    for( i <- 0 until vex.n )
      this.a(i) = vex(i)


  def apply(  i:Int ) : Exp = a(i)
  def update( i:Int, b:Exp ) : Unit = { a(i) = b }

  def + ( v:Vex ) : Vex = 
    { val u = new Vex(n); for( i <- this ) { u(i) = Add(a(i),v(i)) }; u }
  def - ( v:Vex ) : Vex = 
    { val u = new Vex(n); for( i <- this ) { u(i) = Sub(a(i),v(i)) }; u }

  def mul( s:Exp )         : Vex = // Exp * Vex  -- called by Exp
    { val u = new Vex(n); for( i <- this ) { u(i) = Mul(s,a(i)) }; u }

  def * ( v:Vex ) : Vex = // Vex * Vex
    { val u = new Vex(n); for( i <- this ) { u(i) = Mul(a(i),v(i)) }; u }

  override def * ( s:Exp ) : Vex = // Vex * Exp
    { val u = new Vex(n); for( i <- this ) { u(i) = Mul(a(i),s) }; u }

  def * ( b:Mex ) : Vex = // Vex * Mex
  {
    if( n != b.n ) throw new Error()
    val c = new Vex(b.m)
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
    { val u = new Vex(n); val s = Rec(mag); for( i <- this ) { u(i) = Mul(a(i),s) }; u; }

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
    val vex = new Vex(n)
    for( i <- this )
        vex(i) = func( a(i) )
    vex
  } 

}

object Vex
{
  val emp : Vex = new Vex(0)

  def apply( exp:Exp ) : Vex = exp match {
    case vex:Vex  => vex
    case _        => Logg.trace(4, "Bad Cast", exp.toString); emp }

}
