
package ax6.util

//import scala.collection.compat.IterableOnce
import scala.reflect.ClassTag
//import scala.reflect.ClassTag.Nothing


// ------------------------------- Lode[T] -------------------------------------

class Lode[T]( _elem:T )
{
  var elem            : T = _elem               // var elem is mutable
  def term            : Lode[T] = Lode.term
  @transient var prev : Lode[T] = term
  @transient var nexn : Lode[T] = term

  def next()  : T       = nexn.elem
  def hasNext : Boolean = nexn != term
}

object Lode
{
  def term[T] : Lode[T] = null
  def apply[T]( lode:Lode[T] ) : Lode[T] = { new Lode[T]( lode.elem ) }
}

// ---------------------------- object Lyst[T] ---------------------------------

object Lyst
{
  def apply[T](                 ) : Lyst[T] = new Lyst[T]()
  def apply[T]( seq:Seq[T]      ) : Lyst[T] = { val lyst = Lyst[T](); for( t <- seq   ) { lyst.add(t) }; lyst }
  def apply[T]( lysa:Lyst[T]    ) : Lyst[T] = { val lyst = Lyst[T](); for( t <- lysa  ) { lyst.add(t) }; lyst }
  def apply[T]( array:Array[T]  ) : Lyst[T] = { val lyst = Lyst[T](); for( t <- array ) { lyst.add(t) }; lyst }
  def unapplySeq[T]( seq:Seq[T] ) : Option[Seq[T]] = Option(seq)
}

// ----------------------------- class Lyst[T] ---------------------------------

class Lyst[T]()
{
  var size : Int   = 0
  def term            : Lode[T] = Lode.term
  @transient val ring : Lode[T] = term
  ring.prev    = ring
  ring.nexn    = ring

  def head : Lode[T] = ring.nexn
  def tail : Lode[T] = ring.prev

  def in( node:Lode[T])  : Boolean = node!=term && node!=ring
  def in( index:Int )    : Boolean = 0 <= index && index < size

// ... add ins del ...

  private def inc(): Unit = { size += 1 }
  private def dec(): Unit = { size -= 1 }

  // Add to list only if elem is unique
  def put( elem:T ) : Lode[T] =
  {
    val node:Lode[T] = find(elem)
    if(in(node)) node else add(elem)
  }

  def isEmpty: Boolean = { size == 0 }

  def clear(): Unit = {
    var node = head
    var nexn = head
    while( in(node) )
    {
      nexn = node.nexn
      del( node )
      node = nexn
    }
  }   
 
 // .......................................

  def create( elem:T )  : Lode[T]   = new Lode[T](  elem )
  def +=(     elem:T )  : Unit = { add(elem) }
  def add(    elem:T )  : Lode[T]   = add( create(elem) )
  def add( seq:Seq[T] ) : Lode[T]   = { for( elem <- seq ) add(elem); tail }

  private def add( node:Lode[T] ) : Lode[T]=
  {
    if( !in(node) )
      { Log.error( "node not in", node.toString ); return term }
    
    if( isEmpty )        // Add the first node
    {
      node.prev = ring
      node.nexn = ring
      ring.prev = node
      ring.nexn = node
    }
    else // Add node to the tail, ring.prev() is the tail
    {
      node.prev      = ring.prev
      node.nexn      = ring
      ring.prev.nexn = node
      ring.prev      = node
    }
    inc()
    node
  }

  def ins( elem:T ) : Lode[T]= ins( create(elem) )

  private def ins( node:Lode[T] ) : Lode[T] =
  {
    if( !in(node) )
      return term

    if( isEmpty )
      add(node)
    else                   // Insert node to the tail
    {
      node.prev      = ring
      node.nexn      = ring.nexn
      ring.nexn.prev = node
      ring.nexn      = node
      inc()
    }
    node
 }

  def add( pred:Lode[T], elem:T ) : Lode[T]= add( pred, create(elem) )

  private def add( pred:Lode[T], node:Lode[T] ) : Lode[T]=
  {
    if( !in(pred) || !in(node) )
      return term
    
    node.prev      = pred      // Set the node adjacent poInters
    node.nexn      = pred.nexn
    pred.nexn.prev = node      // Reset the poInters around the list node
    pred.nexn      = node
    inc()
    node
  }

  def ins( succ:Lode[T], elem:T ) : Lode[T]= ins( succ, create(elem) )

  private def ins( succ:Lode[T], node:Lode[T] ) : Lode[T]=
  {
    if( !in(succ) || !in(node) )
      return term
    
    node.prev      = succ.prev  // Set the node adjacent poInters
    node.nexn      = succ
    succ.prev.nexn = node       // Reset the poInters around the list node
    succ.prev      = node
    inc()
    node
  }

  def del( elem:T ) : Lode[T]= del( find(elem)   )

  def del( node:Lode[T] ) : Lode[T]=
  {
    if( !in(node) )
      return term
    
    node.prev.nexn = node.nexn
    node.nexn.prev = node.prev
    dec()

    node.nexn = term
    node.prev = term
    node
  }

// ... find ...
  def find( elem:T ) : Lode[T] =
  {
    var node = head
    while( in(node) )
    {
      if( node.elem == elem )
        return node
      node = node.nexn
    }
    term
  }

  def find( idx:Int ) : Lode[T] =
  {
    if( in(idx) )
    {
      var i = 0
      var node = head
      while( in(node) )
      {
        if( i==idx )
          return node
        i += 1
        node = node.nexn
      }
    }
    term
  }

  def indexOf( elem:T ) : Int =
  {
    var i = 0
    var node = head
    while( in(node) )
    {
      if( node.elem == elem )
        return i
      i += 1
      node = node.nexn
    }
    -1
  }

  def indexOf( _node:Lode[T] ) : Int =
  {
    var i = 0
    var node = head
    while( in(node) )
    {
      if( _node==node )
        return i
      i += 1
      node = node.nexn
    }
    -1
  }

  def has( elem:T ) : Boolean = in(find(elem))

  // ... for comprehensions ...
  // foreach map flatMap withFilter

  def foreach( func:T => Unit ): Unit = {
    var node = head
    while( in(node) )
      { func(node.elem); node = node.nexn }
  }

  def map[B]( func:T => B )  : Lyst[B] =
  {
    var node : Lode[T]= head
    val lyst : Lyst[B] = new Lyst[B]()
    while( in(node) )
    {
      lyst.add( func(node.elem) )
      node = node.nexn
    }
    lyst
  }

  def filter( pred:T => Boolean ): Lyst[T] =
  {
    var node = head
    val lyst = new Lyst[T]()
    while( in(node) )
    {
      if( pred(node.elem) )
        lyst.add( Lode[T](node) )
      node = node.nexn
    }
    lyst
  }

  /*
  def flatten[U]( implicit toIterableOnce: T => IterableOnce[U]): Lyst[U] = {
    var node  = head
    val lystU = new Lyst[U]()
    while( in(node) ) {
      node.elem match  {
        case elem:T        => lystU.add(func(elem))
        case seqT:Seq[T]     => for( elem <- seqT   ) lystU.add(elem)
        case lystT:List[T]   => for( elem <- lystT  ) lystU.add(elem)
        case arrayT:Array[T] => for( elem <- arrayT ) lystU.add(elem)
      }
      node = node.nexn
    }
    lystU
  }
  */

  //def flatMap[U]( func: T => IterableOnce[U] ) : Lyst[U] = {
  /*
  def flatMap[U]( func: T => U ) : Lyst[U] = {
    var node = head
    val lystU = new Lyst[U]()
    while( in(node) ) {
      node.elem match  {
        case elem:T          => lystU.add(func(elem))
        case seqT:Seq[T]     => for( elem <- seqT   ) lystU.add(func(elem))
        case lystT:List[T]   => for( elem <- lystT  ) lystU.add(func(elem))
        case arrayT:Array[T] => for( elem <- arrayT ) lystU.add(func(elem))
      }
      node = node.nexn
    }
    lystU
  }
  */
  def withFfilter( pred:T => Boolean ) : Lyst[T] = filter( pred )

  def toArray[B >: T : ClassTag] : Array[B] =
  {
    val array = new Array[B](size)
    var node  = head
    var i     = 0
    while( in(node) ) {
      array(i) = node.elem
      node     = node.nexn
      i = i + 1
    }
    array
  }

  def toList[B >: T : ClassTag] : List[B] = toArray[B].toList

}

/*
  val verm    : Lode[Nothing] = new Lode[Nothing]( null.asInstanceOf[Nothing] )
private class LystIter[T]( _node:Lode[T] ) extends Iterator[T]
{
  var node:Lode[T] = _node
  def term             : Lode[T] = Lode.term
  override def hasNext : Boolean = node!=null && node!=term && node.nexn!=term
  override def next()  : T = { val elem = node.elem; node = node.nexn; elem }
}
 */

