
package ax6.util

//import scala.collection._


// ------------------------------- Lode[T] -------------------------------------

class Lode[D]( dataa:D )
{
  type N       = Lode[D]
  var data : D = dataa
  def term            : Lode[D] = Lyst.term[D]
  @transient var prev : Lode[D] = term
  @transient var next : Lode[D] = term

  def copy() : Lode[D]  = new Lode[D](data)
  def init(): Unit = { prev = term; next = term }
  override def toString  : String  = data.toString
}

private class LystIter[D]( nodea:Lode[D] ) extends Iterator[D]
{
  var node:Lode[D] = nodea
  override def hasNext: Boolean = node!=null && node!=node.term && node.next!=node.term
  override def next() : D = { val data = node.data; node = node.next; data }
}

// ---------------------------- object Lyst[D] ---------------------------------

object Lyst
{
  def term[D]:Lode[D] = new Lode[D]( null.asInstanceOf[Nothing] ) // [Nothing] terminator cast to D
  def apply[D](                 ) : Lyst[D] = new Lyst[D]()
  def apply[D]( seq:Seq[D]      ) : Lyst[D] = { val mist = Lyst[D](); for(t <- seq   ) { mist.add(t) }; mist }
  def apply[D]( array:Array[D]  ) : Lyst[D] = { val mist = Lyst[D](); for(t <- array ) { mist.add(t) }; mist }
  def unapplySeq[D]( seq:Seq[D] ) : Option[Seq[D]] = Option(seq)
}

// ----------------------------- class Lyst[D] ---------------------------------

class Lyst[D]() // extends WithFilter[D,Lyst[D]]
{
  var size : Int   = 0
  @transient val ring:Lode[D] = Lyst.term // new Lode[D](null.asInstanceOf[D])
  ring.prev    = ring
  ring.next    = ring

  @transient val term:Lode[D] = Lyst.term[D]
  def head : Lode[D] = ring.next
  def tail : Lode[D] = ring.prev

  def in( node:Lode[D])  : Boolean = node!=null && node!=ring && node!=term //
  def in( idx:Int )      : Boolean = 0 <= idx && idx < size

// ... Seq ...

  // override def length        : Int         = size
  // override def apply( i:Int) : D           = node(i).data


// ... add ins del ...

  private def inc(): Unit = { size += 1 }
  private def dec(): Unit = { size -= 1 }

  // Add to list only if data is unique
  def put( data:D ) : Lode[D] =
  {
    val node:Lode[D] = find(data)
    if(in(node)) node else add(data)
  }
  
// ........ Heap ........

// def put(   key:K, data:D ) : Lode[D]= add(data)
  def upd(     node:Lode[D], data:D ) : Lode[D] = { node.data = data; node }
  def update(  node:Lode[D], data:D ) : Unit    = { node.data = data       }
// def key(  node:Lode[D],  key:K ) : Lode[D]= node
// def del(  node:Lode[D] )         : Lode[D]// Delete data and key from a location
// def node(  key:K )         : Lode[D]= // node(i) 
// def find(  key:K )         : D = // find(To.ID)

  def peekHead : D = head.data // findMin
  def peekTail : D = tail.data // findMax
   
  def popHead : D =  // deleteMin
  {
    var node : Lode[D]= term
    if( !isEmpty )
      { node = head; del(head) }
    node.data
   }

  def popTail : D =  // deleteMax
  {
      var node : Lode[D]= term
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

  def create( data:D )  : Lode[D]   = new Lode[D](  data )
  def +=(     data:D )  : Unit = { add(data) }
  def add(    data:D )  : Lode[D]   = add( create(data) )
  def add( seq:Seq[D] ) : Lode[D]   = { for( data <- seq ) add(data); tail }

  private def add( node:Lode[D] ) : Lode[D]=
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

  def ins( data:D ) : Lode[D]= ins( create(data) )

  private def ins( node:Lode[D] ) : Lode[D] =
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

  def add( pred:Lode[D], data:D ) : Lode[D]= add( pred, create(data) )

  private def add( pred:Lode[D], node:Lode[D] ) : Lode[D]=
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

  def ins( succ:Lode[D], data:D ) : Lode[D]= ins( succ, create(data) )

  private def ins( succ:Lode[D], node:Lode[D] ) : Lode[D]=
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

  def del( data:D ) : Lode[D]= del( find(data)   )

  def del( node:Lode[D] ) : Lode[D]=
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
  def find( data:D ) : Lode[D] =
  {
    var node = head
    while( in(node) )
    {
      if( node.data == data )
        return node
      node = node.next
    }
    term
  }

  def compare( cmp:Lode[D]=>Boolean ) : Lode[D] =
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

  def node( idx:Int ) : Lode[D]=
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

  def index( _node:Lode[D] ) : Int =
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

  def has( data:D ) : Boolean = in(find(data))



  // ... for comprehensions ...
  // foreach map flatMap withFilter

  def foreach( func:D => Unit ): Unit = {
    var node = head
    while( in(node) )
      { func(node.data); node = node.next }
  }

  def map[B]( func:D => B )  : Lyst[B] =
  {
    var node : Lode[D]= head
    val lyst : Lyst[B] = new Lyst[B]()
    while( in(node) )
    {
      lyst.add( func(node.data) )
      node = node.next
    }
    lyst
  }

  def filter( pred:D => Boolean ): Lyst[D] =
  {
    var node = head
    val hold = new Lyst[D]()
    while( in(node) )
    {
      if( pred(node.data) )
        hold.add( node.copy() )
      node = node.next
    }
    hold
  }

  def flatMap[B]( func:D => B )        : Lyst[B] = map( func )
  def withFfilter( pred:D => Boolean ) : Lyst[D] = filter( pred )

  // def toArray[B>:D] : Array[D] =
  // def toArray[B >: D](implicit arg0: ClassTag[B]): Array[D] =
  /*
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
  */

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

  // ... stack ...

  def pushHead( node:Lode[D] ) : Unit = { ins(node) }
  def pushHead( data:D )       : Unit = { ins(data) }
  def pushTail( node:Lode[D] ) : Unit = { add(node) }
  def pushTail( data:D )       : Unit = { add(data) }

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
  def fwd( beg:Lode[D], f: Lode[D]=> Unit ): Unit = {
    var node = beg
    while( in(node) )
      { f(node); node = node.next }
  }

  def bak( beg:Lode[D], f: Lode[D]=> Unit ): Unit = {
    var node = beg
    while( in(node) )
      { f(node); node = node.prev }
  }

 */