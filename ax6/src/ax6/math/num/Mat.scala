
package ax6.math.num

import  ax6.util.Text
import org.apache.spark.mllib.linalg.{ DenseMatrix => Matrix }

class Mat private ( _mat:Matrix )
{
  var mat:Matrix = _mat
  def n():Int = mat.numCols
  def m():Int = mat.numRows

  def this( n:Int, m:Int, a:Array[Double] )  = this( new Matrix(n,m,a) )
  def this( n:Int, m:Int )                   = this( new Matrix(n,m,Mat.zeroValues(n,m) ) )

  def apply( i:Int, j:Int ) : Double = mat.apply(i,j)
  def a(     i:Int, j:Int ) : Double = mat.apply(i,j)
//def copy() : Mat = new Mat( mat.copy() )

  def row( i:Int ) : Vec =
  {
    val d = new Array[Double](m)
    (0 until m)
      .foreach(j => d(j) = a(i, j))
    new Vec(d)
  }

  def col( j:Int ) : Vec =
  {
    val d = new Array[Double](n)
    (0 until n)
      .foreach(i => d(i) = a(i, j))
    new Vec(d)
  }
  def diag : Vec =
  {
    val d = new Array[Double](n)
    for( i <- 0 until n )
      d(i) = a(i,i)
    new Vec(d)
  }

//def * ( b:Mat  )   : Mat = new Mat( Matrix.multiply(b.mat) )  // multiple is a Java Static

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