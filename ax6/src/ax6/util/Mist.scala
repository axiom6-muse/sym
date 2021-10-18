
package ax6.util

import scala.collection._ //WithFilter

// ------------------------------- Hode[T] -------------------------------------

class Hode[D]( dataa:D )
{
  type N       = Hode[D]
  var data : D = dataa

  def term            : Hode[D] = Mist.term[D]
  @transient var prev : Hode[D] = term
  @transient var next : Hode[D] = term

  def copy() : Hode[D]  = new Hode[D](data)
  def init(): Unit = { prev = term; next = term }
  override def toString  : String  = data.toString
}

private class HoldIter[D]( nodea:Hode[D] ) extends Iterator[D]
{
  var node:Hode[D] = nodea
  override def hasNext: Boolean = node!=null && node!=node.term && node.next!=node.term
  override def next() : D = { val data = node.data; node = node.next; data }
}

// ---------------------------- object Mist[D] ---------------------------------

object Mist
{
  def term[D]:Hode[D] = new Hode[D]( null.asInstanceOf[Nothing] ) // [Nothing] terminator cast to D
  def apply[D](                 ) : Mist[D] = new Mist[D]()
  def apply[D]( seq:Seq[D]      ) : Mist[D] = { val mist = Mist[D](); for(t <- seq   ) { mist.add(t) }; mist }
  def apply[D]( array:Array[D]  ) : Mist[D] = { val mist = Mist[D](); for(t <- array ) { mist.add(t) }; mist }
  def apply[D]( dat:D           ) : Mist[D] = { val mist = Mist[D]();                    mist.add(dat); mist }
  def unapplySeq[D]( seq:Seq[D] ) : Option[Seq[D]] = Option(seq)
}

// ----------------------------- class Mist[D] ---------------------------------

class Mist[D]() // extends WithFilter[D,Mist[D]]
{
//type N = Hode[D]      // Does not work as in the past. Researching an explantion
  var size : Int   = 0

  @transient val ring:Hode[D] = new Hode[D](null.asInstanceOf[D])
  ring.prev    = ring
  ring.next    = ring

  @transient val term:Hode[D] = Mist.term[D]



  def head : Hode[D] = ring.next
  def tail : Hode[D] = ring.prev

  def in( node:Hode[D])  : Boolean = node!=null && node!=ring && node!=term //
  def in( idx:Int )        : Boolean = 0 <= idx && idx < size

// ... Seq ...

  // override def length        : Int         = size
  // override def apply( i:Int) : D           = node(i).data


// ... add ins del ...

  private def inc(): Unit = { size += 1 }
  private def dec(): Unit = { size -= 1 }

  // Add to list only if data is unique
  def put( data:D ) : Hode[D] =
  {
    val node:Hode[D] = find(data)
    if(in(node)) node else add(data)
  }
  
// ........ Heap ........

// def put(   key:K, data:D ) : Hode[D]= add(data)
  def upd(     node:Hode[D], data:D ) : Hode[D] = { node.data = data; node }
  def update(  node:Hode[D], data:D ) : Unit    = { node.data = data       }
// def key(  node:Hode[D],  key:K ) : Hode[D]= node
// def del(  node:Hode[D] )         : Hode[D]// Delete data and key from a location
// def node(  key:K )         : Hode[D]= // node(i) 
// def find(  key:K )         : D = // find(To.ID)

  def peekHead : D = head.data // findMin
  def peekTail : D = tail.data // findMax
   
  def popHead : D =  // deleteMin
  {
    var node : Hode[D]= term
    if( !isEmpty )
      { node = head; del(head) }
    node.data
   }

  def popTail : D =  // deleteMax
  {
      var node : Hode[D]= term
      if( !isEmpty )
          { node = tail; del(tail) }
      node.data
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

  def create( data:D )  : Hode[D]   = new Hode[D](  data )
  def +=(     data:D )  : Unit = { add(data) }
  def add(    data:D )  : Hode[D]   = add( create(data) )
  def add( seq:Seq[D] ) : Hode[D]   = { for( data <- seq ) add(data); tail }

  private def add( node:Hode[D] ) : Hode[D]=
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

  def ins( data:D ) : Hode[D]= ins( create(data) )

  private def ins( node:Hode[D] ) : Hode[D] =
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

  def add( pred:Hode[D], data:D ) : Hode[D]= add( pred, create(data) )

  private def add( pred:Hode[D], node:Hode[D] ) : Hode[D]=
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

  def ins( succ:Hode[D], data:D ) : Hode[D]= ins( succ, create(data) )

  private def ins( succ:Hode[D], node:Hode[D] ) : Hode[D]=
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

  def del( data:D ) : Hode[D]= del( find(data)   )

  def del( node:Hode[D] ) : Hode[D]=
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
  def find( data:D ) : Hode[D] =
  {
    var node = head
    while( in(node) )
    {
      if( node.data.equals(data) )
        return node
      node = node.next
    }
    term
  }

  def compare( cmp:Hode[D]=>Boolean ) : Hode[D] =
  {
    var node = head
    while( in(node) )
    {
      if( cmp(node) )
        return node
      node = node.next
    }
    term
  }

  def node( idx:Int ) : Hode[D]=
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
    Log.trace( 6, "Index", idx, "out of range", size )
    term
  }

  def index( _node:Hode[D] ) : Int =
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
    Log.trace( 6, "index not found for node", _node.toString )
    -1
  }

  def has( data:D ) : Boolean = in(find(data))

// ... stack ...

  def pushHead( node:Hode[D] ) : Unit = { ins(node) }
  def pushHead( data:D )       : Unit = { ins(data) }
  def pushTail( node:Hode[D] ) : Unit = { add(node) }
  def pushTail( data:D )       : Unit = { add(data) }

  // ... for comprehensions ...

  // forNode calls func on each node
  def forNode( func:Hode[D] => Unit ): Unit = {
     var node = head
     while( in(node) )
       { func(node); node = node.next }
  }

  // foreach calls func on each node data
  def foreach( func:D => Unit ): Unit = {
    var node = head
    while( in(node) )
      { func(node.data); node = node.next }
  }

  // Create new List by calling pred p on each List element
  // and then if pred is true places the result in a new list
  def filter( pred:D => Boolean ): Mist[D] =
  {
    var node = head
    val hold = new Mist[D]()
    while( in(node) )
    {
      if( pred(node.data) )
        hold.add( node.copy() )
      node = node.next
    }
    hold
  }

  // def withFilter( p:    (A) => Boolean): WithFilter[A, [_]ListBuffer[_]]

  // def withFilter( pred: (D) => Boolean ): WithFilter[D, [_]Mist[_]]

  //def withFilter(p: D => Boolean): scala.collection.WithFilter[D, Mist[D]] = new IterableOps.WithFilter(this, p)

// Create new List by calling func on each List element
// and then place the result in a new List
  def map[B]( func:D => B )  : Mist[B] =
  {
    var node : Hode[D]= head
    val sold : Mist[B] = new Mist[B]()
    while( in(node) )
    {
      sold.add( func(node.data) )
      node = node.next
    }
    sold
  }

// flatmap flattens List of Lists by creating a new List and then
// calls func the map the element.
// This method hard to Interpret so have to consider its implementation
/*
  def flatMap[B]( func:(D) => Iterable[B] )  : Mist[B] =
  {
     val sold : Mist[B] = new Mist[B]()
     val flat = flatMap[B]( data => func(data) )
     while( flat.hasNext )
       sold.add( flat.next )
     sold
  }
*/


  def toArray : Array[D] =
  {
    val array = new Array[D](size)
    var node  = head
    var i     = 0
    while( in(node) ) {
      array(i) = node.data
      node     = node.next
      i = i + 1
    }
    array
  }

  def toList : List[D] = toArray.toList

  def toListPrepend : List[D] =
  {
    var list = List[D]()
    var node = tail
    while( in(node) )
    {
      list = node.data :: list
      node = node.prev
    }
    list
  }


  def log(): Unit = {
    for( data <- this )
       Log.log( data.toString )
  }

  override def toString : String =
  {
    var str : String = new String
    var node = head
    while( in(node) )
    {
       if( in(node.next) )
         str += node.toString + Text.delim
       else
         str += node.toString
       node = node.next
    }
    str
  }

  def text( tx:Text, b:String, m:String, e:String ) : Text =
  {
    tx.clear()
    tx.app(b)
    var node = head
    while( in(node) )
    {
      tx.app( node.toString )
      if( in(node.next) )
         tx.app( m )
      node = node.next
    }
    tx.app(e)
    tx
  }

  def text( tx:Text  ) : Text = text( tx,        "", Text.delim, "" )
  def text( m:String ) : Text = text( Text(100), "", m,          "" )
  def text()           : Text = text( Text(100), "", Text.delim, "" )
  def show( tx:Text  ) : Text = { tx.clear(); text(tx) }

}

// An good deque or stack based on the Java ArrayDeque

class Deque[T]
{
   private val dq:java.util.ArrayDeque[T] = new java.util.ArrayDeque[T](16)

// ... Stack ...
   def push( e:T ): Unit = { dq.push(e) }
   def pop         : T    = dq.pop
   def peek        : T    = dq.peek

// ... Deque ...
   def pushHead( e:T ): Unit = { dq.push(e) }
   def popHead          : T    = dq.pop
   def peekHead         : T    = dq.peekFirst

   def pushTail( e:T ): Unit = { dq.add(e) }
   def popTail          : T    = dq.removeLast()
   def peekTail         : T    = dq.peekLast

   def isEmpty : Boolean       = dq.isEmpty
   def clear(): Unit = { dq.clear() }
}

/*
  def fwd( beg:Hode[D], f: Hode[D]=> Unit ): Unit = {
    var node = beg
    while( in(node) )
      { f(node); node = node.next }
  }

  def bak( beg:Hode[D], f: Hode[D]=> Unit ): Unit = {
    var node = beg
    while( in(node) )
      { f(node); node = node.prev }
  }

 */