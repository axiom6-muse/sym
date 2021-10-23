
package ax6.util

import scala.reflect.ClassTag


// ------------------------------- Lode[T] -------------------------------------

class Lode[T]( _elem:T )
{
  var elem            : T = _elem                          // var elem is mutable
  def term            : Lode[T] = Lyst.term[T]
  @transient var prev : Lode[T] = term
  @transient var next : Lode[T] = term

  def copy() : Lode[T]  = new Lode[T](elem)
  override def toString  : String  = elem.toString
}

private class LystIter[T]( _node:Lode[T] ) extends Iterator[T]
{
  var node:Lode[T] = _node
  override def hasNext: Boolean = node!=null && node!=node.term && node.next!=node.term
  override def next() : T = { val elem = node.elem; node = node.next; elem }
}

// ---------------------------- object Lyst[T] ---------------------------------

object Lyst
{
  def term[T]:Lode[T] = new Lode[T]( null.asInstanceOf[Nothing] ) // [Nothing] terminator cast to T
  def apply[T](                 ) : Lyst[T] = new Lyst[T]()
  def apply[T]( seq:Seq[T]      ) : Lyst[T] = { val lyst = Lyst[T](); for(t <- seq   ) { lyst.add(t) }; lyst }
  def apply[T]( array:Array[T]  ) : Lyst[T] = { val lyst = Lyst[T](); for(t <- array ) { lyst.add(t) }; lyst }
  def unapplySeq[T]( seq:Seq[T] ) : Option[Seq[T]] = Option(seq)
}

// ----------------------------- class Lyst[T] ---------------------------------

class Lyst[T]()
{
  @transient val term:Lode[T] = Lyst.term[T]
  var size : Int   = 0
  @transient val ring:Lode[T] = term
  ring.prev    = ring
  ring.next    = ring

  def head : Lode[T] = ring.next
  def tail : Lode[T] = ring.prev

  def in( node:Lode[T])  : Boolean = node!=term && node!=ring && node!=null //
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
    var next = head
    while( in(node) )
    {
      next = node.next
      del( node )
      node = next
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
      node.next = ring
      ring.prev = node
      ring.next = node
    }
    else // Add node to the tail, ring.prev() is the tail
    {
      node.prev      = ring.prev
      node.next      = ring
      ring.prev.next = node
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
      node.next      = ring.next
      ring.next.prev = node
      ring.next      = node
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
    node.next      = pred.next
    pred.next.prev = node      // Reset the poInters around the list node
    pred.next      = node
    inc()
    node
  }

  def ins( succ:Lode[T], elem:T ) : Lode[T]= ins( succ, create(elem) )

  private def ins( succ:Lode[T], node:Lode[T] ) : Lode[T]=
  {
    if( !in(succ) || !in(node) )
      return term
    
    node.prev      = succ.prev  // Set the node adjacent poInters
    node.next      = succ
    succ.prev.next = node       // Reset the poInters around the list node
    succ.prev      = node
    inc()
    node
  }

  def del( elem:T ) : Lode[T]= del( find(elem)   )

  def del( node:Lode[T] ) : Lode[T]=
  {
    if( !in(node) )
      return term
    
    node.prev.next = node.next
    node.next.prev = node.prev
    dec()

    node.next = term
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
      node = node.next
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
        node = node.next
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
        lyst.add( node.copy() )
      node = node.next
    }
    lyst
  }

  def flatMap[B]( func:T => B )        : Lyst[B] = map( func )
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

