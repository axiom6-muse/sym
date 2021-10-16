
package ax6.math.num

import  ax6.util.Text
import  Jama._

class Mat( _n:Int, _m:Int )
{
  val n:Int  = _n
  val m:Int  = _m
//val array  = new Array[Double]( n * m )
  val matrix = new Matrix(n,m)
  for( i <- 0 until n )
    for( j <- 0 until m )
      this(i,j) = 0.0

  def apply( i:Int, j:Int ) : Double = matrix.get(i,j)
  def a(     i:Int, j:Int ) : Double = matrix.get(i,j)
  def update( i:Int, j:Int, v:Double ): Unit = { matrix.set(i,j,v) }


  def row( i:Int ) : Vec =
  {
    val r = new Array[Double](m)
     for( j <- 0 until m )
      r(j) = a(i,j)
    Vec(r)
  }

  def col( j:Int ) : Vec =
  {
    val c = new Array[Double](n)
    for( i <- 0 until n )
      c(i) = a(i,j)
    Vec(c)
  }

  def diag : Vec =
  {
    val d = new Array[Double](n)
    for( i <- 0 until n )
      d(i) = a(i,i)
    Vec(d)
  }

  def * ( b:Mat  )   : Mat = Mat( matrix.times(b.matrix) )
  def * ( b:Double ) : Mat = Mat( matrix.times(b) )
  def * ( b:Vec  )   : Vec =
  {
    if( m != b.n )
      throw new Error()
    val c = new Vec(b.n)
    for( i <- 0 until n )
    {
      c(i) = 0.0
      for( j <- 0 until m )
        c(i) = c(i) + a(i,j) * b(j)
    }
    c
  }

  def + ( b:Mat )  : Mat = Mat( matrix.plus( b.matrix) )
  def - ( b:Mat )  : Mat = Mat( matrix.minus(b.matrix) )

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

  // ... for comprehensions ...

  def foreach( func:Double => Unit ): Unit = {
    for( i <- 0 until n )
      for( j <- 0 until m )
        func(a(i,j))
  }

  def map( func:Double => Double ) : Mat =
  {
    val mat = Mat(n,m)
    for( i <- 0 until n )
      for( j <- 0 until m )
        mat(i,j) = func( a(i,j) )
    mat
  }

  // ... Jama LU, QR, SV, EV, CH Decompositions ...

  private def lu:LUDecomposition            = matrix.lu
  private def qr:QRDecomposition            = matrix.qr
  private def sv:SingularValueDecomposition = matrix.svd
  private def ev:EigenvalueDecomposition    = matrix.eig
  private def ch:CholeskyDecomposition      = matrix.chol

  def lul     : Mat = Mat( lu.getL )
  def luu     : Mat = Mat( lu.getU )
  def qrh     : Mat = Mat( qr.getH )
  def qrq     : Mat = Mat( qr.getQ )
  def qrr     : Mat = Mat( qr.getR )
  def svs     : Mat = Mat( sv.getS )
  def svu     : Mat = Mat( sv.getU )
  def svv     : Mat = Mat( sv.getV )
  def eigVec  : Mat = Mat( ev.getV )
  def eigDia  : Mat = Mat( ev.getD )
  def eigValR : Vec = Vec( ev.getRealEigenvalues )
  def eigValI : Vec = Vec( ev.getImagEigenvalues )
  def chl     : Mat = Mat( ch.getL )

}

object Mat {

  val empty:Matrix = Mat(0,0).matrix

  def apply( n: Int, m: Int ): Mat = new Mat(n,m)

  def apply( _mat:Mat ) : Mat = {
    val mat:Mat = Mat( _mat.n, _mat.m )
    for (   i <- 0 until mat.n )
      for ( j <- 1 until mat.m )
        mat(i,j) = _mat(i,j)
    mat
  }

  def apply( matrix:Matrix ) : Mat = {
    val n = matrix.getRowDimension
    val m = matrix.getColumnDimension
    val mat:Mat = Mat( n, m )
    for (   i <- 0 until n )
      for ( j <- 1 until m )
        mat(i,j) = matrix.get(i,j)
    mat
  }
}

/*
  private var lt:LUDecomposition = Mat.empty
  private def lu:LUDecomposition = { if( lt==Mat.empty ) lt = matrix.lu; lt }

  private var qt:QRDecomposition = Mat.empty
  private def qr:QRDecomposition = { if( qt==Mat.empty ) qt = matrix.qr; qt }

  private var st:SingularValueDecomposition = Mat.empty
  private def sv:SingularValueDecomposition = { if( st==Mat.empty ) st = matrix.svd; st }

  private var et:EigenvalueDecomposition = Mat.empty
  private def ev:EigenvalueDecomposition = { if( et==Mat.empty ) et = matrix.eig; et }

  private var ct:CholeskyDecomposition = Mat.empty
  private def ch:CholeskyDecomposition = { if( ct==Mat.empty ) ct = matrix.chol; ct }
 */