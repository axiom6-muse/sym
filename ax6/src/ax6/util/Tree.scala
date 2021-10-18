
package ax6.util
//import  core.util.{ Log, To }
//import  meta.data.{ Persist, Xml }
//import  java.util.concurrent.lock()  s.ReentrantLock
//import  java.util.List

// ------------------------------- Tode[T] -------------------------------------

class Tode[T](  _data:T )
{
  var data   : T     = _data
  var level  : Int   = 0
  def term : Tode[T] = Tree.term[T]
  @transient var prev   : Tode[T] = term
  @transient var next   : Tode[T] = term
  @transient var parent : Tode[T] = term
  @transient var child  : Tode[T] = term
  @transient var last   : Tode[T] = term

  def toNext : Tode[T] = { if( child != term ) child else next   }
  def toPrev : Tode[T] = { if( prev  != term ) prev  else parent }


  def copy( data:T ) : Tode[T] = new Tode[T](data)

  override def toString:String = data.toString

  def init() : Unit =
    { prev = term; next = term; parent = term; child = term; last = term }
}

// ------------------------------- Tree[T] -------------------------------------

class Tree[T]( rootData:T )
{
  var size : Int   = 1

  @transient val root:Tode[T] = new Tode[T](rootData)
  
// ... bounds ...
  def head                 : Tode[T] = root.child
  def head( node:Tode[T] ) : Tode[T] = node.parent.child
  def tail                 : Tode[T] = root.last
  def tail( node:Tode[T] ) : Tode[T] = node.parent.last
  def term                 : Tode[T] = Tree.term[T]
  def in( node:Tode[T] )   : Boolean = node!=term && node!=null

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

  def add( data:T ) : Tode[T] = add( root, new Tode[T](data) )

//private def add( child:Tode[T] ) : Tode[T] = add( root, child )

  def add( parn:T, data:T ) : Tode[T] =
      add( find(parn), data )

  def add( parent:Tode[T], data:T ) : Tode[T] =
      add( parent, new Tode[T](data)  )

  private def add( parent:Tode[T], child:Tode[T] ) : Tode[T] =
  {
    if( !in(parent) || !in(child) )
      return term

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
    child
  }

  def ins( data:T        ) : Tode[T] = ins( root, new Tode[T](data) )
  def ins( child:Tode[T] ) : Tode[T] = ins( root, child )

  def ins( parent:Tode[T],     data:T ) : Tode[T] =
      ins( parent, new Tode[T](data)  )

  private def ins( parent:Tode[T], child:Tode[T] ) : Tode[T] =
  {
    if( !in(parent) || !in(child) )
      return term

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
    child

 }

  def del( data:T       ) : Tode[T] = del( find(data) )
//def del( node:Tode[T] ) : Tode[T] = del() // isSync
  def del(              ) : Tode[T] = del() // isSync

  protected def del( node:Tode[T] ) : Tode[T] =
  {
    if( !in(node) )
      return term

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
    node
  }

  def clear() : Unit =
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

  def clear( node:Tode[T]) : Unit =
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

  def find( data:T ) : Tode[T] =
  {
    for( node <- this )
      if( node.data == data )
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

  def prepost( node:Tode[T] )( pre: Tode[T] => Unit )( post: Tode[T] => Unit ) : Unit =
  {
    var child = node.child
    while( in(child) )
    {
      pre( child )
      prepost( child )( pre )( post )
      post( child )
      child = child.next
    }
  }

  def gen() : Unit =
  {
    for( node <- this )
       Log.tab( node.level-1, node.data.toString )
  }

  def filter( isIn:T => Boolean ): Tree[T] =
  {
    val tree   = new Tree[T](root.data)
    var parent = tree.root
    var fode   = term
    var node   = root.child
    while( in(node) )
    {
      if( isIn(node.data) ) {
        fode = tree.add( parent, node.data )
      }
      parent = fode.parent
      node   = node.toNext
    }
    tree
  }

  def map[B]( func : T => B ) : Tree[B] =
  {
    val tree   = new Tree[B](func(root.data))
    var parent = tree.root
    var fode   = Tree.term[B]
    var node   = root.child
    while( in(node) )
    {
      fode = new Tode[B](func(node.data))
      tree.add( parent, fode )
      parent = fode.parent
      node   = node.toNext
    }
    tree
  }

  /*
  def flatMap[T]( f: (A) => IterableOnce[T]): List[T]
  def flatmap[T]( f: Tode[T] => Iterable[Tode[T]] ) : Hold[T] =
  {
    var node = head
    val hold = new Hold[T]()
    var fode = term
    while( in(node) )
    {
      fode = node // f(node)
      hold.add( new Tode[T](node) )
      node = node.next
    }
    hold
  }
   */
}

object Tree
{
  def term[D]:Tode[D] = new Tode[D]( null.asInstanceOf[Nothing] )
  val maxLevel : Int = 12

  def test() : Unit =
  {
    val tree = new Tree[String]("0")
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


  }
}


/*
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

  def prepost2( top:Tode[T] )( pre: Tode[T] => Unit )( post: Tode[T] => Unit ) : Unit =
  {
    pre(  top )
    recurse2( top )( pre )( post )
    post( top )
  }

  // ... save open
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

