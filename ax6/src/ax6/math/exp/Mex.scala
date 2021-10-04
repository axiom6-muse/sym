
package ax6.math.exp

import  ax6.math.num.Mat

case class Mex( mat:Array[Vex] ) extends Exp
{
  val n:Int = mat.length
  val m:Int = mat(0).n

  def a(      i:Int, j:Int ) : Exp = mat(i*n+j)
  def apply(  i:Int, j:Int ) : Exp = mat(i*n+j)
  def update( i:Int, j:Int,  b:Exp ) : Unit = { mat(i)(j) = b }

  def sub( br:Int, er:Int, bc:Int, ec:Int ) : Mex = 
  {
    val mex:Mex = Mex( er-br+1, ec-bc+1 )
    for( i <- br until er+1 )
      for( j <- bc until ec+1 )
        mex(i-br,j-bc) = a(i,j)
    mex
  }

  def row( i:Int ) : Vex =
  {
    val d = new Array[Exp](m)
    for( j <- 0 until m )
      d(j) = a(i,j)
    Vex(d)
  }

  def col( j:Int ) : Vex = 
  {
    val d = new Array[Exp](n)
    for( i <- 0 until n )
      d(i) = a(i,j)
    Vex(d)
  }

  def diag : Vex =
  {
    val d = new Array[Exp](n)
    for( i <- 0 until n )
      d(i) = a(i,i)
    Vex(d)
  }

  def + ( b:Mex ) : Mex = 
  {
    val c = Mex(n,m)
    for( i <- 0 until n )
      for( j <- 0 until m )
        c(i,j) = Add(a(i,j),b(i,j))
    c
  }

  def - ( b:Mex ) : Mex =
  {
    val c = Mex(n,m)
    for( i <- 0 until n )
      for( j <- 0 until m )
        c(i,j) = Sub(a(i,j),b(i,j))
    c
  }

  def mul ( b:Exp ) : Mex = /// Exp * Mex -- called by Exp
  {
    val c:Mex = Mex(n,m)
    for( i <- 0 until n )
      for( j <- 0 until m )          
        c(i,j) = Mul(b,a(i,j))
    c
  }

  override def * ( b:Exp ) : Mex = // Mex * Exp
  {
    val c:Mex = Mex(n,m)
    for( i <- 0 until n )
      for( j <- 0 until m )          
        c(i,j) = Mul(a(i,j),b)
    c
  }

  def * ( b:Vex ) : Vex = // Mex * Vex
  {
    val c = Vex(b.n)
    for( i <- 0 until n )
      for( j <- 0 until m )
        { c(i) = if(j==0) Mul(a(i,j),b(j)) else Add(c(i),Mul(a(i,j),b(j))) }
    c
  }

  def * ( b:Mex ) : Mex = // Mex * Mex
  {
    val c:Mex = Mex(n,b.m)
    for( i <- 0 until n )
      for( j <- 0 until b.m )
        for( k <- 0 until m )          
          { c(i,j) = if(k==0) Mul(a(i,k),b(k,j)) else Add(c(i,j),Mul(a(i,k),b(k,j))) }
     c
  }

  def transpose : Mex = 
  {
    val mex = Mex( m, n )
    for( j <- 0 until m )
      for( i <- 0 until n )
        mex(j,i) = a(i,j)
    mex
  }


  def calcMex( aa:Assign ) : Mat =
  {
    val mat = Mat(n,m)
    for(   i <- 0 until n )
      for( j <- 0 until m )
        mat(i,j) = a(i,j).calc(aa)
    mat
  }  

// ... for comprehensions ...

  def foreach( func:Exp => Unit ): Unit = {
    for( i <- 0 until n )
      for( j <- 0 until m )
        func(a(i,j))
  } 

  def map( func:Exp => Exp ) : Mex =
  {
    val mex = Mex(n,m)
    for( i <- 0 until n )
      for( j <- 0 until m )
        mex(i,j) = func( a(i,j) )
    mex
  }  
  
  def zero(): Unit = {
    for( i <- 0 until n )
      for( j <- 0 until m )
        this(i,j) = Num(0)
  }  

// ... experimental ....

    def inv2x2  : Mex =
    {
       val inv  = Mex( 2, 2 )
       val fac  = Rec( Sub( Mul(a(0,0),a(1,1)), Mul(a(0,1),a(1,0)) ) )
       inv(0,0) =     Mul(fac,a(1,1));  inv(0,1) = Neg(Mul(fac,a(0,1)))
       inv(1,0) = Neg(Mul(fac,a(1,0))); inv(1,1) =     Mul(fac,a(0,0))
       inv
    }
}

object Mex
{
  //def unapply( mex:Mex ) : Array[Exp] = mex.mat

  def apply( mat:Array[Vex] ) : Mex = new Mex(mat)
  
  def apply( n: Int, m: Int ) : Mex = {
    val mat = new Array[Vex](n)
    for ( i <- 0 until n )
      mat(i) = Vex(m)
    Mex(mat)
  }

  def apply( list:List[Exp] ) : Mex = {
    val head = Vex( list.head )
    val mex:Mex = Mex( list.size, head.n )
    var i = 0
    for (   e <- list ) {
      val vex = Vex(e)
      for ( j <- 1 until mex.m )
        mex(i,j) = vex(j)
    }
    i = i + 1
    mex
  }

  /*
  def apply( list:List[Vex] ) : Mex = {
    val mex:Mex = Mex( list.size, list.head.n )
    var i = 0
    for (   e <- list )
      for ( j <- 1 until mex.m )
        mex(i,j) = e(j)
      i = i + 1
    mex
  }

  def apply( vexs:Array[Vex] ) : Mex = {
    val mex:Mex = Mex( vexs.length, vexs(0).n )
    for (   i <- 0 until mex.n )
      for ( j <- 1 until mex.m )
        mex(i,j) = vexs(i).a(j)
    mex
  }
  */

  def apply( _mex:Mex ) : Mex = {
    val mex:Mex = Mex( _mex.n, _mex.m )
    for (   i <- 0 until mex.n )
      for ( j <- 1 until mex.m )
        mex(i,j) = _mex(i,j)
    mex
  }

  def apply( exp:Exp ) : Mex = exp match  // This a cast
  {
    case mex:Mex  => mex
    case _        => Mex(0,0) // Logg.trace(4, "Bad Cast", exp.toString); emp
  }


}

