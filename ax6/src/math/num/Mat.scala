package ax6.math.num

import scala.Array

class Mat( _mat:Array[Double] ) {
  var mat : Array[Double] = _mat
  def this( n:Int, m:Int )    = this( new Array[Double](n*m) )
  def n:Int = 3
  def m:Int = 3
  def update( i:Int, j:Int, b:Double ): Unit = {  }
  def apply( i:Int, j:Int ) : Double = 3.0

}

object Mat  {

}

