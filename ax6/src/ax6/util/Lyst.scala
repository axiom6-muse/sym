
package ax6.util

import scala.reflect.ClassTag

// ------------------------------- Lode[T] -------------------------------------

class Lode[T]( _elem:T )
{
  var elem : T       = _elem
  var prev : Lode[T] = this
  var next : Lode[T] = this
}

object Lode
{
  // def term[T] : Lode[T] = null.asInstanceOf[Lode[T]]
  def apply[T]( elem:T )       : Lode[T] = { new Lode[T](      elem ) }
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
  val ring  : Lode[T] = Lode(null)
  ring.prev           = ring
  ring.next           = ring
  var size : Int      = 0

  def head : Lode[T] = ring.next
  def tail : Lode[T] = ring.prev

  def in( node:Lode[T])  : Boolean = node != ring && node != ring
  def in( index:Int )    : Boolean = 0 <= index && index < size
  def isEmpty            : Boolean = { size == 0 }
  private def inc()      : Unit    = { size += 1 }
  private def dec()      : Unit    = { size -= 1 }

 // ................. add ......................

  def +=(     elem:T )  : Unit      = { add( elem ) }
  def add(    elem:T )  : Lode[T]   = { add( tail, Lode(elem) ) }
  def add( seq:Seq[T] ) : Lode[T]   = { for( elem <- seq ) add(elem); tail }

  def add( pred:Lode[T], node:Lode[T] ) : Lode[T] =
  {
    if( !in(pred) || !in(node) )
      return ring
    
    if( isEmpty )        // Add the first node
    {
      node.prev = ring
      node.next = ring
      ring.prev = node
      ring.next = node
    }
    else if( pred == tail ) // Add node to the tail, ring.prev() is the tail
    {
      node.prev      = ring.prev
      node.next      = ring
      ring.prev.next = node
      ring.prev      = node
    }
    else
    {
      node.prev      = pred      // Set the node adjacent poInters
      node.next      = pred.next
      pred.next.prev = node      // Reset the poInters around the list node
      pred.next      = node
    }
    inc()
    node
  }

  // Add to list only if elem is unique
  def put( elem:T ) : Lode[T] =
  {
    val node:Lode[T] = find(elem)
    if(in(node)) node else add(elem)
  }

  // ................. ins ......................

  def ins( elem:T ) : Lode[T]= ins( head, Lode(elem) )

  def ins( succ:Lode[T], elem:T ) : Lode[T]= ins( succ, Lode(elem) )

  def ins( succ:Lode[T], node:Lode[T] ) : Lode[T] =
  {
    if( !in(succ) || !in(node) )
      return ring

    if( isEmpty )
      add( tail, node)
    else if( succ == head )                  // Insert node to the head
    {
      node.prev      = ring
      node.next      = ring.next
      ring.next.prev = node
      ring.next      = node
    }
    else
    {
      node.prev      = succ.prev  // Set the node adjacent poInters
      node.next      = succ
      succ.prev.next = node       // Reset the poInters around the list node
      succ.prev      = node
    }
    inc()
    node
 }

  // ................ del ....................

  def del( elem:T ) : Lode[T]= del( find(elem)   )

  def del( node:Lode[T] ) : Lode[T]=
  {
    if( !in(node) )
      return ring
    
    node.prev.next = node.next
    node.next.prev = node.prev
    dec()

    node.next = ring
    node.prev = ring
    node
  }

  def clear(): Unit = {
    var node = head
    var next = head
    while( in(node) )
    {
      next = node.next
      del( node )
      node = next
    }
  }

  // ... find ...
  def find( elem:T ) : Lode[T] =
  {
    var node = head
    while( in(node) )
    {
      if( node.elem == elem )
        return node
      node = node.next
    }
    ring
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
        node = node.next
      }
    }
    ring
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
      node = node.next
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
      node = node.next
    }
    -1
  }

  def has( elem:T ) : Boolean = in(find(elem))

  // ... for comprehensions ...
  // foreach map flatMap withFilter

  def foreach( func:T => Unit ): Unit = {
    var node = head
    while( in(node) )
      { func(node.elem); node = node.next }
  }

  def map[B]( func:T => B )  : Lyst[B] =
  {
    var node : Lode[T]= head
    val lyst : Lyst[B] = new Lyst[B]()
    while( in(node) )
    {
      lyst.add( func(node.elem) )
      node = node.next
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
        lyst.add( lyst.tail, Lode[T](node) )
      node = node.next
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
      node = node.next
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
      node = node.next
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
      node     = node.next
      i = i + 1
    }
    array
  }

  def toList[B >: T : ClassTag] : List[B] = toArray[B].toList

}

/*

  def lode[U>:T]( lode:U ) : Lode[T] = lode.asInstanceOf[Lode[T]]
  def term[Null>:T]        : Lode[T] = null.asInstanceOf[Lode[T]]

  val verm    : Lode[Nothing] = new Lode[Nothing]( null.asInstanceOf[Nothing] )
private class LystIter[T]( _node:Lode[T] ) extends Iterator[T]
{
  var node:Lode[T] = _node
  def term             : Lode[T] = Lode.term
  override def hasNext : Boolean = node!=null && node!=term && node.next!=term
  override def next()  : T = { val elem = node.elem; node = node.next; elem }
}
 */

