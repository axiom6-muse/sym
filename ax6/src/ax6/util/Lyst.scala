
package ax6.util

import scala.reflect.ClassTag

// ------------------------------- Lode[T] -------------------------------------

class Lode[I]( _elem:I )
{
  var elem : I       = _elem
  var prev : Lode[I] = this
  var next : Lode[I] = this
}

object Lode
{
  // def term[I] : Lode[I] = null.asInstanceOf[Lode[I]]
  def apply[I]( elem:I )       : Lode[I] = { new Lode[I](      elem ) }
  def apply[I]( lode:Lode[I] ) : Lode[I] = { new Lode[I]( lode.elem ) }
}

// ---------------------------- object Lyst[I] ---------------------------------

object Lyst
{
  def apply[I](                 ) : Lyst[I] = new Lyst[I]()
  def apply[I]( seq:Seq[I]      ) : Lyst[I] = { val lyst = Lyst[I](); for( t <- seq   ) { lyst.add(t) }; lyst }
  def apply[I]( lysa:Lyst[I]    ) : Lyst[I] = { val lyst = Lyst[I](); for( t <- lysa  ) { lyst.add(t) }; lyst }
  def apply[I]( array:Array[I]  ) : Lyst[I] = { val lyst = Lyst[I](); for( t <- array ) { lyst.add(t) }; lyst }
  def unapplySeq[I]( seq:Seq[I] ) : Option[Seq[I]] = Option(seq)
}

// ----------------------------- class Lyst[I] ---------------------------------

class Lyst[I]()
{
  val ring  : Lode[I] = Lode(null)
  ring.prev           = ring
  ring.next           = ring
  var size : Int      = 0

  def head : Lode[I] = ring.next
  def tail : Lode[I] = ring.prev

  def in( node:Lode[I] )         : Boolean = node != ring
  def in( node:Option[Lode[I]] ) : Boolean = node != None 
  def in( index:Int )            : Boolean = 0 <= index && index < size

  def isEmpty       : Boolean = { size == 0 }
  private def inc() : Unit    = { size += 1 }
  private def dec() : Unit    = { size -= 1 }

 // ................. add ......................

  def +=(  elem:I )               : Unit      = { add( elem ) }
  def add( elem:I )               : Lode[I]   = { add( tail, Lode(elem) ) }
  def add( pred:Lode[I], elem:I ) : Lode[I]   = { add( pred, Lode(elem) ) }
  def add( seq:Seq[I] )           : Lode[I]   = { for( elem <- seq ) add(elem); tail }

  def add( pred:Lode[I], node:Lode[I] ) : Lode[I] =
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
  def put( elem:I ) : Lode[I] =
  {
    val node:Lode[I] = find(elem)
    if( in(node) ) ring else add(elem)
  }

  // ................. ins ......................

  def ins( elem:I ) : Lode[I]= ins( head, Lode(elem) )

  def ins( succ:Lode[I], elem:I ) : Lode[I]= ins( succ, Lode(elem) )

  def ins( succ:Lode[I], node:Lode[I] ) : Lode[I] =
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

  def del( elem:I ) : Lode[I]= del( find(elem)   )

  def del( node:Lode[I] ) : Lode[I]=
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

  // By returning val ring as not found, find(elem) works nicely with in(node)
  def find( elem:I ) :  Lode[I] =
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

  def findOption( elem:I ) : Option[Lode[I]] =
  {
    var node = head
    while( in(node) )
    {
      if( node.elem == elem )
        return Some(node)
      node = node.next
    }
    None
  }

  def find( idx:Int ) : Lode[I] =
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

  def indexOf( elem:I ) : Int =
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

  def indexOf( _node:Lode[I] ) : Int =
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

  def has( elem:I ) : Boolean = in(find(elem))

  // ... for comprehensions ...
  // foreach map flatMap filter withFilter

  def foreach( func:I => Unit ): Unit = {
    var node = head
    while( in(node) )
      { func(node.elem); node = node.next }
  }

  def map[U]( func:I => U )  : Lyst[U] =
  {
    var node : Lode[I]= head
    val lyst : Lyst[U] = Lyst[U]()
    while( in(node) )
    {
      lyst.add( func(node.elem) )
      node = node.next
    }
    lyst
  }

  // Verify
  def flatMap[U]( func: I => IterableOnce[U] ) : Lyst[U] = {
    var node : Lode[I] = head
    val lyst : Lyst[U] = Lyst[U]()
    while( in(node) ) {
      val iter = func(node.elem).iterator
      while( iter.hasNext ) {
        lyst.add( iter.next() )
      }
      node = node.next
    }
    lyst
  }

  def filter( pred:I => Boolean ): Lyst[I] =
  {
    var node = head
    val lyst = Lyst[I]()
    while( in(node) )
    {
      if( pred(node.elem) )
        lyst.add( lyst.tail, Lode[I](node) )
      node = node.next
    }
    lyst
  }


  def withFfilter( pred:I => Boolean ) : Lyst[I] = filter( pred )

  def toArray[B >: I : ClassTag] : Array[B] =
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

  def toList[B >: I : ClassTag] : List[B] = toArray[B].toList

}

/*

  def lode[U>:I]( lode:U ) : Lode[I] = lode.asInstanceOf[Lode[I]]
  def term[Null>:I]        : Lode[I] = null.asInstanceOf[Lode[I]]

  val verm    : Lode[Nothing] = new Lode[Nothing]( null.asInstanceOf[Nothing] )
private class LystIter[I]( _node:Lode[I] ) extends Iterator[I]
{
  var node:Lode[I] = _node
  def term             : Lode[I] = Lode.term
  override def hasNext : Boolean = node!=null && node!=term && node.next!=term
  override def next()  : I = { val elem = node.elem; node = node.next; elem }
}

  def flatten[U]( implicit toIterableOnce: I => IterableOnce[U]): Lyst[U] = {
    var node  = head
    val lystU = new Lyst[U]()
    while( in(node) ) {
      node.elem match  {
        case elem:I        => lystU.add(func(elem))
        case seqT:Seq[I]     => for( elem <- seqT   ) lystU.add(elem)
        case lystT:List[I]   => for( elem <- lystT  ) lystU.add(elem)
        case arrayT:Array[I] => for( elem <- arrayT ) lystU.add(elem)
      }
      node = node.next
    }
    lystU
  }


  //def flatMap[U]( func: I => IterableOnce[U] ) : Lyst[U] = {

  def flatMap[U]( func: I => U ) : Lyst[U] = {
    var node = head
    val lystU = new Lyst[U]()
    while( in(node) ) {
      node.elem match  {
        case elem:I          => lystU.add(func(elem))
        case seqT:Seq[I]     => for( elem <- seqT   ) lystU.add(func(elem))
        case lystT:List[I]   => for( elem <- lystT  ) lystU.add(func(elem))
        case arrayT:Array[I] => for( elem <- arrayT ) lystU.add(func(elem))
      }
      node = node.next
    }
    lystU
  }

 */

