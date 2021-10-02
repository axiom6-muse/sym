
package ax6.math.num

import  ax6.util.Text

class Mat( _n:Int, _m:Int )
{
  val n:Int = _n
  val m:Int = _m
  val array = new Array[Double]( n * m )
  for( k <- 0 until n*m )
    array(k) = 0.0

  def apply( i:Int, j:Int ) : Double = array.apply(i+n+j)
  def a(     i:Int, j:Int ) : Double = array.apply(i+n+j)

  def row( i:Int ) : Vec =
  {
    val r = new Array[Double](m)
     for( j <- 0 until m )
      r(j) = a(i,j)
    new Vec(r)
  }

  def col( j:Int ) : Vec =
  {
    val c = new Array[Double](n)
    for( i <- 0 until n )
      c(i) = a(i,j)
    new Vec(c)
  }
  def diag : Vec =
  {
    val d = new Array[Double](n)
    for( i <- 0 until n )
      d(i) = a(i,i)
    new Vec(d)
  }

  def toStr : String =
  {
    val t:Text = new Text( n*(m*6+4) )
    t.all( '[' )
    for( i <- 0 until n )
    {
      t.all('[', a(i, 0))
      for( j <- 1 until m )
        t.all(',', a(i, j))
      t.all(']')
    }
    t.app(']')
    t.toStr
  }

}

object Mat
{
  def zeroValues( n:Int, m:Int ) : Array[Double] = {
    val a: Array[Double] = new Array[Double]( n * m )
    for (i <- 0 until n)
      for (j <- 0 until m)
        a(i*n+j) = 0.0
    a
  }
}