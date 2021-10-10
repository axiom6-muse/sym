
package ax6.util
//import  core.util.{ Log, To }
//import  meta.data.{ Persist, Xml }
//import  java.util.concurrent.lock()  s.ReentrantLock
//import  java.util.List

// ------------------------------- Tode[T] -------------------------------------

class Tode[T]( _tid:Long, _nid:Long, _data:T )
{
  val tid    : Long = _tid
  val nid    : Long = _nid
  var data   : T     = _data
  var level  : Int   = 0

  @transient var prev   : Tode[T] = term
  @transient var next   : Tode[T] = term
  @transient var parent : Tode[T] = term
  @transient var child  : Tode[T] = term
  @transient var last   : Tode[T] = term

  def this(  _tid:Long, _data:T ) = { this( _tid, To.id, _data ) }

  def term : Tode[T] = To.to[Tode[T]](Term)
  def copy( _tid:Long ) : Tode[T] = new Tode[T](_tid,data)

  def eq( node:Tode[T] ) : Boolean = { nid == node.nid }

  override def toString:String = data.toString

  def init() : Unit =
    { prev = term; next = term; parent = term; child = term; last = term }
}

object Term extends Tode[Nothing]( 0, 0, To.to[Nothing](null) ) {}

// ------------------------------- Tree[T] -------------------------------------

class Tree[T]( _id:Long ) //extends Sync
{
  val tid  : Long = _id  // Assign unique id
  var size : Int   = 0

  @transient val root:Tode[T] = new Tode[T](tid,0,To.to[T](null))

  def this() = this( To.id )

// ... save open
  /*
  def save( persist:Persist ) : Unit =
  {
     persist.put( this )
     for( node <- this )     // Will new prePost for Xml
        persist.put( node )
  }

  def open( persist:Persist ) : Unit =
  {
     var stack : Array[Tode[T]] = new Array[Tode[T]](Tree.maxLevel)
     stack(0) = root

     var list : List = persist.query( root.getClass, tid )
     var iter = list.iterator
     var node : Tode[T]   = term
     while( iter.hasNext )
     {
        node = To.to[Tode[T]](iter.next)
        node.init
        add( stack(node.level-1), node )
        stack(node.level) = node
     }
  }
  */
  
// ... bounds ...
  def lock()   : Unit = {}
  def unlock() : Unit = {}
  def head                 : Tode[T] = root.child
  def head( node:Tode[T] ) : Tode[T] = node.parent.child
  def tail                 : Tode[T] = root.last
  def tail( node:Tode[T] ) : Tode[T] = node.parent.last
  def term                 : Tode[T] = To.to[Tode[T]](Term)
  def in( node:Tode[T] )   : Boolean = node!=null && node!=term

  def isRoot(   node:Tode[T] ) : Boolean = in(node)      &&  node == root
  def isHead(   node:Tode[T] ) : Boolean = in(node)      &&  node == head(node)
  def isTail(   node:Tode[T] ) : Boolean = in(node)      &&  node == tail(node)
  def isBranch( node:Tode[T] ) : Boolean = !isRoot(node) &&  in(node.child)
  def isLeaf(   node:Tode[T] ) : Boolean = !isRoot(node) && !in(node.child)

// ... params ...

  def isEmpty : Boolean = size == 0

  def level( node:Tode[T] ) : Int =
  {
    var lev  = 0; var parent = node.parent
    while( in(parent) )
     { lev = lev + 1; parent = parent.parent }
    node.level = lev
    lev
  }

  def numChild( node:Tode[T] ) : Int =
  {
    var num   = 0
    var child = node.child
    while( in(child) )
      { num = num + 1; child = child.next }
    num
  }

// ... inc dec ...

  private def inc( node:Tode[T] ) : Unit =
  {
    size += 1
    node.level = node.parent.level + 1
  }

  private def dec() : Unit =   // dec( node:Tode[T] )
  {
    size -= 1
  }

// ... add ins del ...

  def add( data:T        ) : Tode[T] = add( root, new Tode[T](tid,data) )

//private def add( child:Tode[T] ) : Tode[T] = add( root, child )

  def add( parn:T, data:T ) : Tode[T] =
      add( find(parn), data )

  def add( parent:Tode[T],     data:T ) : Tode[T] =
      add( parent, new Tode[T](tid,data)  )

  private def add( parent:Tode[T], child:Tode[T] ) : Tode[T] =
  {
    if( !in(parent) || !in(child) )
      return term

    lock()  
    try
    {
      child.parent = parent
      if( !in(parent.child) )
      {
        parent.child = child
        parent.last  = child
      }
      else if( parent.child == parent.last )
      {
        child.prev        = parent.child
        parent.last       = child
        parent.child.next = parent.last
        parent.last.prev  = parent.child
      }
      else     // Add after tail child
      {
        child.prev       = parent.last
        parent.last.next = child
        parent.last      = child
      }
      inc(child)
    }
    finally unlock()
    child
  }

  def ins( data:T        ) : Tode[T] = ins( root, new Tode[T](tid,data) )
  def ins( child:Tode[T] ) : Tode[T] = ins( root, child )

  def ins( parent:Tode[T],     data:T ) : Tode[T] =
      ins( parent, new Tode[T](tid,data)  )

  private def ins( parent:Tode[T], child:Tode[T] ) : Tode[T] =
  {
    if( !in(parent) || !in(child) )
      return term

    lock()  
    try
    {
      child.parent = parent
      if( !in( parent.child ) )
      {
        parent.child = child
        parent.last  = child
      }
      else     // Insert before head child
      {
        child.next        = parent.child
        parent.child.prev = child
        parent.child      = child
      }
      inc(child)
    }
    finally unlock()
    child

 }

  def del( data:T       ) : Tode[T] = del( find(data) )
//def del( node:Tode[T] ) : Tode[T] = del() // isSync
  def del(              ) : Tode[T] = del() // isSync

  protected def del( node:Tode[T] ) : Tode[T] =
  {
    if( !in(node) )
      return term

    lock()
    try
    {
      // Reset parent references
      if( node.parent.child == node )    // Parent child
        { node.parent.child =  node.next }
      if( node.parent.last  == node )    // Parent last
        { node.parent.last  =  node.prev }

      // Reset sibling references
      if( in(node.prev) )                     // Sibling prev
        { node.prev.next = node.next }
      if( in(node.next) )                     // Sibling next
        { node.next.prev = node.prev }

      dec()
    }
    finally unlock()
    node
  }

  def clear() : Unit =
  {
    lock()  
    try
    {
      var node  = root.child
      var next  = term
      while( in(node) )
      {
        next = node.next
        clear( node )
        node = next
      }
    }
    finally unlock()
  }

  def clear( node:Tode[T]) : Unit =
  {
    lock()
    try
    {
      var child = node.child
      var next  = term
      while( in(child) )
      {
        next  = child.next
        clear( child )
        child = next
      }
      del( node )
    }
    finally unlock()
  }

  def find( data:T ) : Tode[T] =
  {
    for( node <- this )
      if( node.data.equals(data) )
        return node
    term
  }

  def find( nid:Long ) : Tode[T] =
  {
    for( node <- this )
      if( node.nid==nid )
        return node
    term
  }

// ... breath traversals ...

  def cousin( node:Tode[T] ) : Tode[T] =
  {
    if(   in(node.next) )
      return node.next

    var ancest = node.parent
    val nlevel = level(node)
    var couson = term

    while( in(ancest) )  // Search for the next cousin
    {
      if( !in( ancest.next ) )
      {
        ancest = ancest.parent
      }
      else
      {
        ancest = ancest.next
        couson = ancest
        var i  = level(ancest)
        while( i < nlevel && in(couson) )
          { couson = couson.child; i = i + 1 }
        if( in(couson) && level(couson) == nlevel )
          return couson
      }
    }
    term
  }

  def cousins( first:Tode[T], visit: Tode[T] => Unit ) : Tode[T] =
  {
    var grandChild : Tode[T] = term
    var child = first
    while( in(child) )
    {
       visit(child)
       if( !in(grandChild) && in(child.child) )
          grandChild = child.child
       child  = cousin(child)
    }
    grandChild
  }

  def breath( top:Tode[T], visit: Tode[T] => Unit ) : Unit =
  {
    visit(top)
    var first = top.child
    while( in(first ) )
      first = cousins( first, visit )
  }

// ... depth traversals ...

  def foreach( visit: Tode[T] => Unit ) : Unit =
  {
    var node = head
    while( in(node) )
    {
      visit(node)
      recurse( node )( visit )
      node = node.next
    }
  }

  def depth( top:Tode[T] )( visit: Tode[T] => Unit ) : Unit =
  {
    visit(top)
    recurse( top )( visit )
  }

  def recurse( node:Tode[T] )( visit: Tode[T] => Unit ) : Unit =
  {
    var child = node.child
    while( in(child) )
    {
      visit(child)
      recurse( child )( visit )
      child = child.next
    }
  }

  def prepost( top:Tode[T] )( pre: Tode[T] => Unit )( post: Tode[T] => Unit ) : Unit =
  {
    pre(  top )
    recurse2( top )( pre )( post )
    post( top )
  }

  def recurse2( node:Tode[T] )( pre: Tode[T] => Unit )( post: Tode[T] => Unit ) : Unit =
  {
    var child = node.child
    while( in(child) )
    {
      pre( child )
      recurse2( child )( pre )( post )
      post( child )
      child = child.next
    }
  }

  def gen() : Unit =
  {
    for( node <- this )
       Log.tab( node.level-1, node.data.toString )
  }

  def nextRecurse( node:Tode[T] ) : Tode[T] =
  {
    if(     !in( node        ) )  term
    else if( in( node.child  ) )  node.child
    else if( in( node.next   ) )  node.next
    else if( in( node.parent ) )  node.parent
    else                          term
  }

  def prevRecurse( node:Tode[T] ) : Tode[T] =
  {
    if(     !in( node        ) )  term
    else if( in( node.prev   ) )  node.prev
    else if( in( node.parent ) )  node.parent
    else                          term
  }

}

object Tree
{
   val maxLevel : Int = 12

   def test() : Unit =
   {
     val tree = new Tree[String]
     var node = tree.term
     node = tree.add("1")
      tree.add( node, "1.1" )
      tree.add( node, "1.2" )
      tree.add( node, "1.3" )
     node = tree.add("2")
      tree.add( node, "2.1" )
      tree.add( node, "2.2" )
      tree.add( node, "2.3" )
     node = tree.add("3")
      tree.add( node, "3.1" )
      tree.add( node, "3.2" )
      tree.add( node, "3.3" )
   // tree.gen

   //val db = new meta.data.Db4D("tree.db4o" )

    //tree.save( db )

      val told = new Tree[String]( tree.tid )
    //told.open( db )
      told.gen()

   }
}

// ... for comprehensions ...
/*
  def filter( f: Tode[T] => Boolean ): Hold =
  {
    var node = head
    var hold = new Hold()
    while( in(node) )
    {
      if( f(node) )
        hold.add( new Tode[T](node) )
      node = node.next
    }
    return hold
  }

  def map( f: Tode[T] => Tode[T] )  : Hold =
  {
    var node = head
    var hold = new Hold()
    var fode = term
    while( in(node) )
    {
      fode = f(node)
      hold.add( new Tode[T](fode) )
      node = node.next
    }
    return hold
  }

  def flatmap( f: Tode[T] => Iterable[Tode[T]] ) : Hold =
  {
    var node = head
    var hold = new Hold()
    var fode = term
    while( in(node) )
    {
      fode = node // f(node)
      hold.add( new Tode[T](node) )
      node = node.next
    }
    return hold
  }
*/