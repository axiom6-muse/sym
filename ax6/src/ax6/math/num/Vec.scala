
package ax6.math.num

import  ax6.util.Text

class Vec( _n:Int ) {
  var array = new Array[Double](_n)
  val n:Int = array.length

  def a(      i:Int )           : Double = array(i)
  def apply(  i:Int )           : Double = array(i)
  def update( i:Int, b:Double ) : Unit = { array(i) = b }

  def ^ ( a:Double, n:Int ):Double = Math.pow(a,n)
  def + ( v:Vec    ) : Vec =
    { val u = Vec(n); for( i <- this ) { u(i) = a(i) + v(i) }; u; }
  def - ( v:Vec    ) : Vec =
    { val u = Vec(n); for( i <- this ) { u(i) = a(i) - v(i) }; u; }
  def dot ( v:Vec    ) : Double =
    { var dot:Double = 0.0; for( i <- this ) { dot = dot + a(i) * v(i) }; dot; } // Dot   product
  def *   ( v:Vec    ) : Double = dot(v)
  def *   ( s:Double ) : Vec = map( a => a*s )
  //  { var u = new Vec(n); for( i <- this ) { u(i) = a(i) * s }; return u; }
  def *   ( b:Mat    ) : Vec =
  {
      if( n != b.n ) throw new Error()
      val c = Vec(b.m)
      for( j <- c )
      {
        c(j) = 0.0
        for( i <- this )
          c(j) = c(j) + a(j) * b(i,j)
      }
      c
  }

  def cos( b:Vec ):Double = ( this dot b ) / ( mag * b.mag )

  def cross( b:Vec ) : Vec =
  {
      val u = Vec(n)
      n match
      {
          case 3 => u(0) = a(1)*b(2)-a(2)*b(1)
                    u(1) = a(2)*b(0)-a(0)*b(2)
                    u(2) = a(0)*b(1)-a(1)*b(0)
          case _ =>
      }
      u
   }

  def mag  : Double =
    { var m:Double = 0.0; for( i <- this ) { m += a(i)*a(i) }; Math.sqrt(m); }
  def Unit : Vec =
    { val u = new Vec(n); val s = 1.0/mag; for( i <- this ) { u(i) = a(i)*s }; u; }

  def text : Text =
  {
      val t:Text = new Text(n*6+4)
      t.all('[', a(0))
      for( i <- 1 until n )
          t.all(',', a(i))
      t.app(']')
      t
  }
  def str() : String = text.toString
  override def toString: String = str()

   // ... comprehensions ...

   // return each i = 0; i < n
   def foreach( f:Int => Unit ): Unit = {
     var i = 0
     while( i < n )
       { f(i); i+=1 }
   }

    def map( func:Double => Double ) : Vec = {
      val vec = new Vec(n)
      for( i <- this )
        vec(i) = func( a(i) )
      vec
    }


}

object Vec {

  def apply( n:Int )            : Vec = new Vec(n)

  def apply( _array:Array[Double] ) : Vec = {
    val vec:Vec = Vec( _array.length)
    for( i <- 0 until vec.n ) vec(i) = _array(i)
    vec
  }
  def apply( list:List[Double] ) : Vec = {
    val vec:Vec = Vec( list.size )
    var i = 0
    for( e <- list ) { vec(i) = e; i = i + 1 }
    vec
  }

  def apply( args:Double* ) : Vec = {
    val vec:Vec = Vec( args.length )
    var i = 0
    for( arg <- args ) { vec(i) = arg; i = i + 1 }
    vec
  }

  def apply( _vec:Vec ) : Vec = {
    val vec:Vec = Vec( _vec.n )
    for( i <- 0 until vec.n ) vec(i) = _vec(i)
    vec
  }
}